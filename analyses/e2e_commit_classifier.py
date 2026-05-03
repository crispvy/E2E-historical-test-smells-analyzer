import argparse
import sqlite3
from pathlib import Path


DB_DEFAULTS = {
	"js": "historical_smellsJS.db",
	"ts": "historical_smellsTS.db",
}

NO_SMELL = "NO_SMELL"

IMPROVING_KEYWORDS = [
	"fix", "improve", "refactor", "cleanup",
	"optimize", "simplify", "remove",
	"update", "enhance", "stabilize"
]

WORSENING_KEYWORDS = [
	"temporary", "temp", "hack", "workaround",
	"disable", "skip", "debug",
	"quick fix", "hotfix", "bypass"
]


def _log_status(message: str) -> None:
	print(f"[status] {message}", flush=True)


def _resolve_db_path(db_name: str) -> Path:
	script_dir = Path(__file__).resolve().parent
	candidates = [
		Path(db_name),
		script_dir / db_name,
		script_dir.parent / db_name,
		script_dir.parent.parent / db_name,
	]

	for candidate in candidates:
		if candidate.exists() and candidate.is_file():
			return candidate.resolve()

	tried = "\n".join(str(path.resolve()) for path in candidates)
	raise FileNotFoundError(f"Database not found: {db_name}. Tried:\n{tried}")


def _normalize(value: str | None, fallback: str = "") -> str:
	if value is None:
		return fallback
	cleaned = str(value).strip()
	return cleaned if cleaned else fallback


def _table_exists(conn: sqlite3.Connection, table_name: str) -> bool:
	return bool(
		conn.execute(
			"SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = ?",
			(table_name,),
		).fetchone()
	)


def _ensure_keyword_columns(conn: sqlite3.Connection) -> None:
	columns = conn.execute("PRAGMA table_info(report_commit_details)").fetchall()
	existing_names = {row[1] for row in columns}

	if "match_keyword_improving" not in existing_names:
		conn.execute(
			"ALTER TABLE report_commit_details ADD COLUMN match_keyword_improving TEXT NOT NULL DEFAULT 'false'"
		)
	if "match_keyword_worsening" not in existing_names:
		conn.execute(
			"ALTER TABLE report_commit_details ADD COLUMN match_keyword_worsening TEXT NOT NULL DEFAULT 'false'"
		)


def _load_commit_message_map(conn: sqlite3.Connection) -> dict[str, str]:
	rows = conn.execute(
		"""
		SELECT
			LOWER(TRIM(h.commit_hash)) AS commit_hash_key,
			COALESCE(NULLIF(TRIM(h.commit_message), ''), '') AS commit_message
		FROM historical_smells h
		INNER JOIN (
			SELECT LOWER(TRIM(commit_hash)) AS commit_hash_key, MIN(rowid) AS first_rowid
			FROM historical_smells
			WHERE TRIM(COALESCE(commit_hash, '')) <> ''
			GROUP BY LOWER(TRIM(commit_hash))
		) first_rows
			ON LOWER(TRIM(h.commit_hash)) = first_rows.commit_hash_key
		   AND h.rowid = first_rows.first_rowid
		"""
	).fetchall()

	return {row[0]: _normalize(row[1], "") for row in rows if row[0]}


def _match_improving(message: str) -> str:
	normalized = message.lower()
	return "true" if any(keyword in normalized for keyword in IMPROVING_KEYWORDS) else "false"


def _match_worsening(message: str) -> str:
	normalized = message.lower()
	return "true" if any(keyword in normalized for keyword in WORSENING_KEYWORDS) else "false"


def _update_keyword_matches(conn: sqlite3.Connection, dataset: str, commit_message_map: dict[str, str]) -> int:
	rows = conn.execute(
		"""
		SELECT rowid, LOWER(TRIM(COALESCE(commit_hash, ''))) AS commit_hash_key
		FROM report_commit_details
		WHERE dataset = ?
		""",
		(dataset,),
	).fetchall()

	total_rows = len(rows)
	_log_status(f"Righe report_commit_details da aggiornare: {total_rows}")

	updates: list[tuple[str, str, int]] = []
	for index, (rowid, commit_hash_key) in enumerate(rows, start=1):
		message = commit_message_map.get(_normalize(commit_hash_key, ""), "")
		updates.append((_match_improving(message), _match_worsening(message), rowid))
		if index % 1000 == 0 or index == total_rows:
			_log_status(f"Preparazione aggiornamenti: {index}/{total_rows}")

	_log_status("Scrittura aggiornamenti nel database...")

	conn.executemany(
		"""
		UPDATE report_commit_details
		SET match_keyword_improving = ?,
			match_keyword_worsening = ?
		WHERE rowid = ?
		""",
		updates,
	)
	return len(updates)


def main() -> None:
	parser = argparse.ArgumentParser(
		description="Classify commit messages from historical_smells SQLite DB by keyword matching."
	)
	parser.add_argument(
		"--dataset",
		required=True,
		choices=("js", "ts"),
		help="Dataset to analyze: js or ts.",
	)
	args = parser.parse_args()

	db_path = _resolve_db_path(DB_DEFAULTS[args.dataset])
	_log_status(f"Dataset selezionato: {args.dataset}")
	_log_status(f"Database risolto: {db_path}")

	conn = sqlite3.connect(str(db_path))
	try:
		_log_status("Connessione al database aperta")
		_log_status("Verifica tabelle richieste...")
		if not _table_exists(conn, "historical_smells"):
			raise RuntimeError("Table historical_smells not found in database.")
		if not _table_exists(conn, "report_commit_details"):
			raise RuntimeError("Table report_commit_details not found in database.")
		_log_status("Tabelle trovate")

		_log_status("Controllo/creazione colonne match keyword...")
		_ensure_keyword_columns(conn)
		_log_status("Caricamento commit message da historical_smells...")
		commit_message_map = _load_commit_message_map(conn)
		_log_status(f"Commit hash caricati: {len(commit_message_map)}")
		_log_status("Inizio classificazione keyword improving/worsening...")
		updated_rows = _update_keyword_matches(conn, args.dataset, commit_message_map)
		_log_status("Commit transazione sul database...")
		conn.commit()
		_log_status("Completato")
	finally:
		conn.close()

	print(f"Updated table report_commit_details in: {db_path}")
	print(f"Dataset: {args.dataset}")
	print(f"Rows updated: {updated_rows}")


if __name__ == "__main__":
	main()
