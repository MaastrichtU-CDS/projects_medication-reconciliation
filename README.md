# Medication Reconciliation

This project aims in generating a shortlist of patients which are likely to
need a reassessment for their medication police.

## Dependencies

* Python >= 3.8.3

## How to run

1. You can build a virtual environment to execute the code by running the 
   `create_virtualenv.sh` script:

    ``` shell
    $ ./create_virtualenv.sh
    ```

    Another option is to install the packages in the `requirements.txt` file.

2. Create a `data` folder and upload the patients data (`QZExport99-Pivot.xlsx`) 
   to it. The data is not public therefore we are not commiting it to `git`.
   
3. Run the main python script:

    ``` shell
    $ python main.py
    ```

## Extras

- `notebooks`: directory with Jupyter notebooks for exploratory analysis and 
  visualizations.
  
## Todo

- [ ] Read report from previous analysis
- [ ] Perform data exploration
- [ ] Train logistic regression
