# DM01 Data Processing Project

The DM01 Data Processing Project is designed to automate the ingestion and uploading of data to the Sandpit as part of a reporting pipeline

Data Files: Ensure that the necessary Excel data files are present in the data directory, organized by date into Flex and Freeze subdirectories.
Database Access: Ensure that you have the necessary permissions and access to the SQL Server database. The script uses an ODBC connection, so you may need to configure your ODBC settings to connect to the database. 
Input CSV Files: Ensure that the necessary input CSV files (Provider_indicator_list.csv, Provider_inconsistency_adjustment.csv, and Sheet_parameters.csv) are present in the 

Detailed information can be found in the supporting documentation in the "docs" folder. 

## To use this template, please use the following practises:

* Put any data files in the `data` folder.  This folder is explicitly named in the .gitignore file.  A further layer of security is that all xls, xlsx, csv and pdf files are also explicit ignored in the whole folder as well.  ___If you need to commit one of these files, you must use the `-f` (force) command in `commit`, but you must be sure there is no identifiable data.__
* Save any documentation in the `docs` file.  This does not mean you should avoid commenting your code, but if you have an operating procedure or supporting documents, add them to this folder.
* Please save all output: data, formatted tables, graphs etc. in the output folder.  This is also implicitly ignored by git, but you can use the `-f` (force) command in `commit` to add any you wish to publish to github.



This repository is dual licensed under the [Open Government v3]([https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) & MIT. All code can outputs are subject to Crown Copyright.
