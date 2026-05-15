# 🔍 E2E Historical Test Smells Analyzer

![Python](https://img.shields.io/badge/Python-3.8+-blue)
![R](https://img.shields.io/badge/R-4.5.3+-red)
![Status](https://img.shields.io/badge/Status-Research_Prototype-orange)
![Purpose](https://img.shields.io/badge/Purpose-Empirical_Research-green)

# 📄 Overview

The **E2E Historical Test Smells Analyzer** is a research-oriented framework designed to support empirical studies on the evolution of **End-to-End (E2E) Web GUI test smells** software repositories from E2EGit dataset.

The project combines:

- **Static analysis** for E2E Web GUI test smell detection
- **Historical repository mining** through Git commit analysis
- **Structured persistence** using SQLite databases
- **Empirical and statistical analysis** using Python and R

The framework focuses on understanding:

- how E2E test smells evolve over time
- when smells are introduced or removed
- how developers interact with smelly test code
- how smells relate to release cycles and ownership

---

# 🎯 Research Goals

The main objectives of this project are:

- detect E2E test smells in JavaScript and TypeScript test suites
- analyze their historical evolution across commits
- reconstruct smell introduction/removal timelines
- collect structured datasets for empirical software engineering research for reconstruction of smell introduction/removal timelines and quantitative and qualitative analyses of developer behavior

---

# 🧪 Supported Smells

- Absolute URL
- Absolute XPath
- Assertion Roulette
- Complex Test
- Conditional Logic
- Constructor Initialization
- Duplicate Assert
- Empty Test
- Exception Handling
- Global Variable
- Magic Number
- Misused Tag Locator
- Mystery Guest
- Non-Preferred Locator
- Redundant Assertion
- Redundant Print
- Sensitive Equality
- Sleepy Test
- Unknown Test
- Unstable Link Text

---

# 🏗️ System Architecture

```text
Repository Dataset
        ↓
Static Smell Detection
        ↓
CSV Intermediate Results
        ↓
Historical Mining (PyDriller)
        ↓
SQLite Databases
        ↓
Empirical Analysis & Visualization
```

---

# ⚙️ Requirements

- Python 3.8+
- pip
- Git
- R 4.5.3+

---

# 🚀 Setup

### 1. Static Analysis
Download the repository [**e2e-test-smell-analyzer**](https://github.com/squidslab/e2e-test-smell-analyzer) as a `.zip` file. Extract the archive, copy the folder into this project directory and then pen a terminal and move inside the folder.

Follow the instructions contained in the **README of the detector** in order to install its dependencies and configure the environment.

Once configured, run the detector to generate the following files: 

```
typescript_analysis.csv
javascript_analysis.csv
```

These files contain the files (and their respective repository) where **E2E Web GUI test smells** were detected.

### 2. Historical Analysis
> ⚠️ **WARNING**  
> This script may take a long time to execute as it needs to clone the repositories one by one. It is strongly recommended to use the **E2E_NUM_WORKERS** parameter (see below) to speed up execution and to have at least **150GB** of free space available. Once you have run the historical analyzer on one of the two languages, continue to Chapter 3 of this readme and then run the historical analyzer for the other language.

Move to the root directory of this project and install dependecies with:

```
pip install -r requirements.txt
```

Then execute one of the following scripts:

```
python history_smells-analyzerJS.py
python history_smells-analyzerTS.py
```

The system generates one of the two SQLite databases:

```
historical_smellsJS.db
historical_smellsTS.db
```

The databases include information such as:

* repository name
* test file path
* framework
* commit SHA
* commit date
* commit author
* commit message
* smell type
* nearest and earliest future release date
* nearest and earliest future release version
* class name
* method name
* line number

This databases will be used later for **empirical analysis** scripts.

#### ✴️ *Environment Variables (Optional)*

The tool supports several environment variables to control the analysis.

| Variable             | Description                                       |
| -------------------- | ------------------------------------------------- |
| `E2E_TEST_MODE`      | Enables test mode with a reduced dataset          |
| `E2E_TEST_MAX_REPOS` | Maximum number of repositories to analyze         |
| `E2E_NUM_WORKERS`    | Number of parallel workers                        |
| `E2E_MAX_COMMITS`    | Maximum number of commits analyzed per repository |
| `E2E_DB_PATH`        | Path/name of the output database                  |
| `E2E_CLONE_ROOT`     | Directory where repositories are cloned           |

---

### 3. Configure empirical analysis tools

> ⚠️ **WARNING**  
> This script requires the cloned repositories downloaded in step 2.

Now move inside the `analyses/` folder. Before executing the empirical analysis scripts, it is necessary to run the **e2e_smells_analyzer.py** which processes the databases generated during the historical analysis phase to perform a further level of data aggregation and transformation, producing three derived datasets:

* report_summary
* report_commit_details
* report_developer_details

These outputs form the input for subsequent empirical analysis scripts, allowing further investigation of the collected data through the use of various graphs, tables and textual reports.

This scripts can be run in two ways:

#### ⏩ Batch execution (all files with smells, recommended):**

```
python analyses/e2e_smells_analyzer.py --dataset js/ts
```
#### ▶️ Single execution (for a repository / specific file):**

```
python analyses/e2e_smells_analyzer.py <repository> <file_name> --dataset js/ts
```
#### ✴️ Main parameters
* `<repository>`: repository name as stored in the DB
* `<file_name>`: path or name of the test file
* `--dataset`: required, `js` or `ts`
* `--db`: (optional) path to the SQLite database
* `--report-db`: (optional) output DB path for report tables (default: same DB selected with `--db`/`--dataset`)
* `--output`: (optional, single mode) output `.txt` path
* `--write-txt`: (optional, batch mode) also generates `.txt` reports

### 4. Empirical analysis tools

#### 4.1 e2e_commit_classifier.py

Still in the `analyses/` folder, run then:

```
python analyses/e2e_commit_classifier.py --dataset js
python analyses/e2e_commit_classifier.py --dataset ts
```

This script performs a keyword-based classification of commit messages to enrich the empirical dataset with additional behavioral signals. 

Specifically, it analyzes commit messages stored in the **report_commit_details** table and checks whether they contain predefined keywords associated with two categories: improving and worsening changes.

No external output files are generated; instead, the enriched information is directly written back into the database. During execution, the script provides console logs to track progress, including dataset selection, database path, number of rows processed, and update/commit operations.

#### 4.2 e2e_smells_report_plots.py

After creating textual reports with `analyses/e2e_smells_analyzer.py`, this script generates a complete set of 5 charts for each report file:

1. **Smell distribution**

![Smell distribution bar](assets/1_smell_distribution_bar.png)

2. **Smell evolution over time (with release lines)**

![Smell evolution line](assets/2_smell_evolution_line.png)

3. **Ownership plot**

![Ownership plot](assets/3_ownership_plot.png)

4. **Smells vs release distance**

![Smells vs release distance](assets/4_smells_vs_release_distance.png)

5. **Smell co-occurrence heatmap**

![Smell co-occurrence heatmap](assets/5_smell_cooccurrence_heatmap.png)

Plots are saved under `analyses/plots/ts/` and `analyses/plots/js/`, inside one folder per repository/file (`<repo>_<file>`).

This scripts can be run in two ways:

#### ▶️ Single execution (from one report):

```
python analyses/e2e_smells_report_plots.py --report analyses/reports/ts/<report_file>.txt
```

#### ⏩ Batch execution (all reports):

```
python analyses/e2e_smells_report_plots.py --reports-dir analyses/reports
```
#### 4.3 e2e_empirical_analyzer.R
This script (analyses/e2e_empirical_analyzer.R) performs the main empirical analysis phase of the project, using the processed datasets stored in historical_smellsJS.db and historical_smellsTS.db

The output consists of both visual and tabular artifacts, including:

* ridge plot visualizations of release distance distributions;
* summary plots of release cycle proximity;
* tables describing test smell distributions and startup-phase behavior;
* ownership and newcomer analysis plots by bad practice;
* CSV reports detailing bad smell incidence across languages and frameworks (only with the use of --incidence-only).

#### ✴️ Main options for `e2e_empirical_analyzer.R`:
* `--ridge-release-only`
* `--smells-only`
* `--startup-tables-only`
* `--ownership-tables-only`
* `--incidence-only`

#### 4.4 quantitative_occurrence_plots.R

> ⚠️** Prerequisite** 
> For this scripts CSV reports from e2e_empirical_analyzer.R are required. If the CSV files are missing, the plotting script cannot run.

For each language, this script generates for each language:

* occurrences by smell type
* distinct tests by smell type
* total occurrences by framework
* framework × smell heatmap (occurrences)
* framework × smell heatmap (distinct tests)

# 🚹 Contributors

* Vincenzo Di Nardo