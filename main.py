# -*- coding: utf-8 -*-

"""
Medication reconciliation project
"""

import os
import numpy as np
import pandas as pd


def load_data(cwd):
    """ Load and prepare data

    Parameters
    ----------
    cwd : str
        Project working directory

    Returns
    -------
    df : pd.DataFrame
        Processed data
    """

    # Read data
    file_path = os.path.join(cwd, 'data', 'QZExport99-Pivot.xlsx')
    df = pd.read_excel(file_path, skiprows=[0,1,2,3], engine='openpyxl')

    # Columns
    cols = {
        'ID': 'ID',
        'A1a': 'gender',
        'A2a': 'age',
        'A10a': 'current_diseases', # split categories
        'A9a': 'number_diseases',
        'A12a': 'kidney_function', # numerical -> categories
        'A14a': 'hospital_visits', # numerical -> categories, last 12 months
        'A150a': 'specialties_mumc', # split categories, last 12 months
        'A16a': 'hospitalization', # yes/no last 12 months
        'A18a': 'n_ae_visits', # last 12 months
        'C1a': 'specialties_visits', # CHECK THAT ONE C1
        'C3a': 'clinic_visit_frequency', # 1st, 2nd, 3rd+ times
        'B1a': 'n_medications', # numerical
        'B7a': 'risky_substances', # split categories
        'B6a': 'presence_risky_substance', # yes/no
        'A3b': 'housing_type', # categories
        'A21b': 'using_medication', # yes/no
        'A4b': 'pharmacy_package_availability', # yes/no
        'A5b': 'medication_giver', # categories
        'A6b': 'education_level', # categories
        'A13b': 'alergies', # yes/no
        'A20b': 'alergy_complaints', # free text
        'A140b': 'visit_other_hospital', # yes/no
        'A150b': 'specialties_not_mumc', # split categories
        'A16b': 'hospitalization_not_mumc', # yes/no
        'A18b': 'n_ae_visits_not_mumc', # numerical
        'B2b': 'self_medication', # split? categories
        'A7c': 'health_assessement', # categories
        'B10c': 'medication_prescription', # yes/no
        'B11c': 'prescribed_medication', # free text
        'B12c': 'prescribed_new_medication', # yes/no
        'B20c': 'stop_medication', # yes/no
        'B21c': 'stopped_medication', # free text
        'B13c': 'no_drug_intervention', # yes/no
        'S1c': 'evs_conflict', # categories
        'S2c': 'medication_interactions', # yes/no
        'S31c': 'g_standard_code1', # categories
        'S41c': 'wfg_evaluation1', # categories
        'S51c': 'changed_police1', # yes/no -> outcome
        'S32c': 'g_standard_code2', # categories
        'S42c': 'wfg_evaluation2', # categories
        'S52c': 'changed_police2', # yes/no -> outcome
        'S33c': 'g_standard_code3', # categories
        'S43c': 'wfg_evaluation3', # categories
        'S53c': 'changed_police3', # yes/no -> outcome
        'S6c': 'changed_police_based_reconciliation', # categories -> outcome
        'C5a': 'no_change_police_risk_cat', # categories -> outcome?
        'C4a': 'reconciliation_relevant_information' # categories -> outcome?
    }

    # Select and rename columns
    df = df[cols.keys()]
    df.rename(columns=cols, inplace=True)

    # Split columns
    split_cols = {
        'current_diseases': np.arange(1, 9),
        'specialties_mumc': np.arange(1, 20),
        'risky_substances': np.arange(1, 13),
        'specialties_not_mumc': np.arange(1, 20),
        'self_medication': np.arange(1, 8)
    }
    for col, cats in split_cols.items():
        for i in cats:
            new_col = '%s_%d' % (col, i)
            df[new_col] = df[col].apply(lambda x: 1 if str(i) in str(x) else 0)
        df.drop(columns=[col], inplace=True)

    # Get dummy variables for other categorical columns
    dummy_cols = ['gender', 'hospitalization', 'presence_risky_substance',
                  'housing_type', 'using_medication',
                  'pharmacy_package_availability', 'medication_giver',
                  'education_level', 'alergies', 'visit_other_hospital',
                  'hospitalization_not_mumc', 'health_assessement',
                  'medication_prescription', 'prescribed_new_medication',
                  'stop_medication', 'no_drug_intervention', 'evs_conflict',
                  'medication_interactions', 'g_standard_code1',
                  'wfg_evaluation1', 'g_standard_code2', 'wfg_evaluation2',
                  'g_standard_code3', 'wfg_evaluation3']
    df = pd.get_dummies(df, columns=dummy_cols)

    return df


if __name__ == '__main__':

    # Working directory
    cwd = os.getcwd()

    # Load and prepare data
    df = load_data(cwd)
    df = df[~df['changed_police_based_reconciliation'].isnull()]
    df.to_csv('data/med_recon.csv', encoding='utf-8', index=False)


