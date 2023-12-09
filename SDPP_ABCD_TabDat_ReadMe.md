Standard Data Preprocessing Pipeline for ABCD Tabulated Data
================
Kunru Song
2023-12-10

- [Introduction](#introduction)
- [Citation and Suggestions](#citation-and-suggestions)
- [Change Logs](#change-logs)

# Introduction

**SDPP-ABCD-TabDat** (**S**tandard **D**ata **P**reprocessing
**P**ipeline for the **A**dolescent **B**rain **C**ognitive
**D**evelopment study **T**abulated **D**ata) is an open-source tool
aiming to provide an user-friendly data preprocessing pipeline for
tabulated data from the ABCD study. This tool mainly depends on R. I
hope this open-source tool could improve the accuracy of the secondary
data analysis with the ABCD study and also provide an easy way to
reproduce results and findings related to the ABCD study data.

**Author:** Kunru Song, PhD Candidate at BNU, supervised by Dr.Jin-tao
Zhang.

[Kunru’s Researchgate
Profile](https://www.researchgate.net/profile/Kunru-Song)

[Kunru’s Google Scholar
Profile](https://scholar.google.com/citations?hl=zh-CN&user=P6j5frUAAAAJ)

[Dr. Jin-tao Zhang’s Lab
(Researchgate)](https://www.researchgate.net/lab/Jintao-Zhang-Lab-2)

**First Created Date:** 2022.06

**Data Version:** ABCD 5.0 Data Release (tabulated data)

**ABCD Links**

- [ABCD 5.0 Tabulated Release
  Data](https://nda.nih.gov/study.html?id=2147)
- [ABCD 5.0 Data Dictionary](https://data-dict.abcdstudy.org/)
- [ABCD 5.0 Release Notes & Update
  Logs](https://wiki.abcdstudy.org/release-notes/start-page.html)

# Citation and Suggestions

1.  If you would like to use any codes in this open-source tool
    (licensed by GNU GPL v3), please kindly follow terms and conditions
    in GNU GPL v3 or later and cite our published article used this tool
    (below is a APA-format Citation):

    > *Song K, Zhang JL, Zhou N, Fu Y, Zou B, Xu LX, Wang Z, Li X, Zhao
    > Y,Potenza M, Fang X, Zhang JT. Youth Screen Media Activity
    > Patterns and Associations With Behavioral Developmental Measures
    > and Resting-state Brain Functional Connectivity. J Am Acad Child
    > Adolesc Psychiatry.2023 Mar 20:S0890-8567(23)00132-6.doi:
    > 10.1016/j.jaac.2023.02.014.Epub ahead of print. PMID: 36963562.*

2.  Please refer to SDPP-ABCD-TabDat **User Manual** when you have any
    questions about using this pipeline. **User Manual** was wrote by R
    Markdown syntax and output as a html file for convenient to be read.
    Users could find the manual in this repository:
    *SDPP_ABCD_TabDat_UserManual.html*.

    In addition, [find here for online html file
    preview](http://htmlpreview.github.io/?https://github.com/fenmeng123/SDPP_ABCD_TabDat/blob/master/SDPP_ABCD_TabDat_UserManual.html).(Supported
    by htmlpreview.github.io)

3.  Any bugs found in this pipeline can be reported in [Github Issues of
    this
    repository](https://github.com/fenmeng123/SDPP_ABCD_TabDat/issues).

4.  If you would like to participate the collaboration in developing
    this pipeline, please kindly create a pull requests in [relevant
    page of this
    repository](https://github.com/fenmeng123/SDPP_ABCD_TabDat/pulls)

# Change Logs

- 2023.12.10

  - Completed SDPP_Batch, implement click-to-run interface.
  - Separate user-defined necessary arguments to a new
    SDPP_UserDefined_Input.R script.
  - TO DO: 1) check the full pipeline and fix bugs; 2) write a new usage
    notes section in user mannual; 3) write some basic code examples in
    README.

- 2023.12.9

  - Completed Step 13 & 14
  - Drafting the first version of SDPP_Batch
  - refractoring some basic functions in SDPP_subfunctions, add more
    high-level warps for convenient callback.
  - Reduce the complexity of SDPP_Paraset.
  - Add sMRI variable names mapping table in **/.github** folder.
  - TO DO: 1) more flexible SDPP_Batch; *2) integrate SDPP_Batch with
    SDPP_Paraset;* 3) check the efficacy of SDPP-ABCD-TabDat on ABCD
    data release V5.1

- 2023.12.7

  - Completed Step 12.
  - Start Step 13 for sMRI tabulated data preprcossing, including
    Destrieux (aparc.a2009s) and subcortical auto-segmentation (aseg)
    atlas.
  - Add new ggseg look up table in **/.github** folder.

- 2023.12.6

  - Add compatability of nomenclature for ggseg label based on ABCD ROI
    names
  - Add new MRI ROI Look Up Table (LUT) for Desikan-Killiany Atlas in
    **/.github** folder.

- 2023.11.28

  - Start Step 13 (extract sMRI-related variables).
  - Add an EXCEL template: SDPP-MRI-Look-up-table
    (*.github/SDPP_MRI_ROI_LookUpTable.xlsx*)
  - Update README & User Manual for ABCD MRI tabulated data.

- 2023.11.27

  - Update README and User Manual for Step 11.
  - Fixed some bugs in *SDPP_ParaSet.R* and *SDPP_Concatenate.R*
  - Start & completed Step 12.
  - Add some paragraphs to clarify the rationale of mutliple imputaion
    startergy used in this pipeline.
  - TO DO: *1) re-construct some basic warp functions in
    SDPP_subfunction.R;* *2) Check the robustness of Step_3;* 3)
    Integrate **neuroCombat** into this pipeline.

- 2023.11.25

  - Completed Step 11.
  - Update User Manual for Step 11.

- 2023.11.24

  - Completed Step 10.
  - Update User Manual for Step 10.
  - Update README
  - Start Step 11.

- 2023.11.23

  - Update README and User Manual for Step 10.
  - Change license to GNU GPL v3.

- 2023.11.22

  - find some bugs and start codes re-constructing to improve
    readability of all codes in SDPP-ABCD-TabDat.
  - Add new features: filter columns by manually modified ABCD Data
    Dictionary file.
  - Update Step 10.
  - Update User Manual for Step 9.

- 2023.9.21

  - Update Step 9.

- 2023.8.7

  - Start Step 11.

- 2023.8.5

  - Completed Step 9.
  - Update User Manual for Non-NIH toolbox Neurocognition Tasks data
    re-coding.
  - Start Step 10.

- 2023.7.31

  - Update ReadMe and User Manual, auto-load EXCEL file to display
    tables
  - Start Step 9

- 2023.7.18

  - Completed Step 8.
  - Add more information about Neurocognition and Mental Health to User
    Manual.
  - Update the Interactive Data Dictionary Excel File.

- 2023.7.17

  - Completed Step 6, add a new subfunction to re-code nm and nt values
    in ABCD Summary Data File.
  - Updated coding scheme table, more columns (e.g. coding details about
    factor levels collapse will be added in future version).
  - Completed Step 7.

- 2023.7.10

  - Updated README and User Manual.

- 2023.7.9

  - Completed Step 5, added some subfunctions to perform knn-imputation
    for missing values in SMA-related behavioral measures.
  - Added coding scheme table.
  - Fixed some bugs in calling MATLAB Bioinformatics Toolbox.

- 2023.07.07

  - Completed Step 4 and write Usage Notes in README, which provide a
    comprehensive overview for using SDPP-ABCD-TabDat.
  - Update some codes to generate more elegant code running logs, which
    would be saved into *‘Res_1_Logs’* folders under your project
    directory. SDPP_ParaSet will generate a log with postfix 0. The
    numeric postfix is correspond to the step number.
  - Separate SDPP-ABCD-TabDat User Manual from README.

- 2023.07.06

  - Modifed Step 4 for ABCD V5.0. Added a new section in README to
    illstruate some details in Step 4.
  - Screen Use Time now will be re-coded into 7-level oridinal variable,
    data type is numeric.

- 2023.07.05

  - Modifed Step 3 for ABCD V5.0.
  - Created a new script *‘SDPP_ParaSet.R’*, which is the *all in one*
    parameters setting script for SDPP-ABCD-TabDat. *‘SDPP_ParaSet.R’*
    should be executed before any steps of SDPP-ABCD-TabDat to source
    all common parameters and libary all common packages. Log for
    *‘SDPP_ParaSet.R’* will be save into Log_SDPP-ABCD-TabDat_0.txt

- 2023.07.03

  - Errors in measures of height and weight that noted in *‘ABCD 5.0
    Changes and Known Issues’* (Page 9) were processed in Step 1 R
    script. please see attached file *‘ABCD5.0_BMI_NotesTable.xlsx’*,
    where Sheet ‘NotesTable’ was directly copied from *‘ABCD 5.0 Changes
    and Known Issues.doc’* (Page 9 & 10), sheet ‘ManualCorrectiveTable’
    was manually generated by Kunru Song (2023.07.03) to correct the
    value with each subject in it. The second sheet could be used to
    replace (‘correct’) the measured values in
    /physical-health/ph_y_anthro.csv (that is what Step 1 would do).
    Please download and place it within the folder where
    SDPP-ABCD-TabDat scripts were storage.
  - At 4-year follow-up, the EHIS was measured again. To keep
    consistent, youth’s handedness indicated by EHIS was determined by
    the baseline measure, which would be seen as a time-invariant
    variable.(Section 8 in Step 1)
  - Add function to automatedly save logs.
  - Merge Intersex-Male and Male into Male. Because the number of
    intersex-male subjects is too small.

- 2023.07.01

  - Add some codes to re-code rel_group_id into a new numeric variable,
    which could describe the order of youths from same family. A new
    variable named GroupID will be created within execution of Step1,
    range from 0 to 3. For single-child family, the value would be set
    to 0. For multi-child family (sibling, twin or triplet), the
    corresponding value indicates the order of youths in their family.
  - Old Variables with prefix *‘GeneInfo’* now were replaced by *‘GI’*.
  - The top five Principal component of genetic ancestry would be
    preserved in the preprocess data frame. I did this as a conventional
    choice such as aCompcor and tCompcor, where we usually extract the
    top five principal components. It should be noted that such
    conventional choice did not have particular reason. Usually, the top
    five PCs are enough to capture the variations in raw data. Because I
    did not find proper justification for ABCD gentic ancestry PCs, such
    choice may be an optional and practical process for the genetic
    ancestry data. Any comments are welcome to address this issue.

- 2023.06.29

  - Revising all codes for ABCD V5.0 Tabulated Data (NDA Collection ID:
    2147)
  - In ABCD 5.0 tabulated data, *‘src_subject_id’* would be used as
    participant identifier.

- 2023.06.06

  - To improve compatability of CSV files across different environment
    (R, Python, MATLAB, LibreOffice, Excel), vaiable names (i.e., Column
    Names) have been modified. All ‘.’ were replaced by ’\_’.

- 2023.06.05

  - Add additional parameter in write.csv with fileEncoding=‘UTF-8’. Now
    all CSV-files generated by SDPP-ABCD-TabDat will be formated in
    UTF-8 encoding type.

- 2023.06.03

  - To keep consistent with NDA platform, the variable *‘subjectkey’*
    will be used as Subject ID to merge data. Because *‘subjectkey’* is
    the *‘NDAR Global Unique Identifier (GUID) for research subject’*.
    Anyone using ABCD data could easily apply GUID filter to create
    their own NDA Study, which is the requirements of ABCD DUC terms and
    condition.
  - Added more comments into R script. Added code blocks to make
    everything easy to read.

------------------------------------------------------------------------
