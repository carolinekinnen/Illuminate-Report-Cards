
# Illuminate Report Cards

## Project Intro/Objective

This project contains a collection of scripts that will generate final grades and GPAs from Illuminate grade exports. It will also aggregate data not available in Illuminate from BigQuery. Output is saved in the directory folders and then should be uploaded into Illuminate for quarterly report cards. There are other systems that also need the final grade data generated here, such as Deans List for progress reports and Powerschool for transcripts. 

### Technologies
* R
* ProjectTemplate
* Google Big Query
* Google Cloud Storage

### Data Systems
* Illuminate
* Powerschool
* Deans List


## Project File Outline:
```
.
└── ISBE_Student_Courses
    |
    ├── README.md                   <- Description of project content.
    |
    ├── config                      <- Contains configuration files for project
    |                                  ProjectTemplate. List required libraries
    |                                  and scripts during `load.project()`.
    ├── data                        
    │   ├── 01-manual_tables.R      <- Load manual tables
    │   ├── 02-bq_files.R           <- Load files from Big Query Database
    │   ├── 03-flat_files.R         <- Loads flat files from Google Cloud Storage (GCS)
    │   ├── flatfiles               <- Contains flatfiles downloaded from GCS
    │   ├── 04-manual_kinder_co     <- Contains a manual table of co-teachers that needs to be updated yearly or upon request by schools
    │   └── README.md               
    |                                
    ├── munge
    |   ├── 01-course_names_munge.R   <- Clean and format data pertaining to course and teacher names
    |   ├── 02-grades_munge.R         <- Clean and format data pertaining to grades and GPA
    |   |── 03-attendance_munge.R     <- Clean and format data pertaining to attendance
    |   └── README.md
    |
    ├── src
    |   ├── course_teacher_name.R
    |   ├── final_grades.R
    |   ├── course_teacher_name.R
    |   ├── deanslist_upload.R
    |   ├── final_grades.R
    |   ├── KTC_data.R
    |   ├── powerschool_transcript_upload.R
    |   ├── quarter_number.R
    |   ├── retention_data.R
    |   ├── promotion_8th.R
    |   └── README.md
    |
    ├── lib                         
    |   └── helpers.R                <- All functions
    |
    ├── output
    │   └── write_file.R            <- Location to write all files to local directory
    |
    ├── ISBE_Student_Courses.Rproject
    |
    └── .gitignore                   <- contains files that should not be
                                        uploaded to github

```
