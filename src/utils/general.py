#!python3.8

import pandas as pd
import requests
import time
import os
import glob
import yaml

from pandas import json_normalize


def read_files(path):
    "Read files"
    df = pd.concat(map(pd.read_parquet, glob.glob(path + "/*.parquet")))

    return df

def read_files_2(path):
    "Read files"
    all_files = glob.glob(os.path.join(path, "*.parquet"))

    df = pd.concat((pd.read_parquet(f) for f in all_files), ignore_index=True)

    return df



def create_folder(directory):
    """Create folder"""
    # Create target Directory if don't exist
    if not os.path.exists(directory):
        os.mkdir(directory)
        print("Directory ", directory, " created ")
    else:
        print("Directory ", directory, " already exists")


def facebook_credentials(creds_file):
    """Get credentials from the credentials file"""
    creds = read_yaml_file(creds_file)

    creds = creds["facebook"]["token"]

    return creds


def read_yaml_file(yaml_file):
    """Load yaml cofigurations"""

    config = None
    try:
        with open(yaml_file, "r") as f:
            config = yaml.safe_load(f)
    except ValueError:
        print("Couldnt load the file")

    return config


def expand_column(df, column):

    df0 = df[~df[column].isna()]
    df0 = df0.reset_index(drop=True)
    dfi = pd.DataFrame(df0[column].tolist())
    try:
        dfi = dfi[0].apply(pd.Series)
    except:
        pass
    df0 = pd.concat([df0, dfi], axis=1)
    df0 = df0.reset_index(drop=True)
    df1 = df[df[column].isna()].reset_index(drop=True)
    df = pd.concat([df0, df1], ignore_index=True)
    df = df.reset_index(drop=True)

    return df


def twitter_credentials(creds_file):
    """Get credentials from the credentials file"""
    creds = read_yaml_file(creds_file)

    creds_jm = creds["twitter"]["bearer_token_jm"]
    creds_ab = creds["twitter"]["bearer_token_ab"]
    creds_hl = creds["twitter"]["bearer_token_hl"]
    creds_ma = creds["twitter"]["bearer_token_ma"]
    creds_jb = creds["twitter"]["bearer_token_jb"]
    creds_dp = creds["twitter"]["bearer_token_dp"]
    creds_ez = creds["twitter"]["bearer_token_ez"]

    return creds_jm, creds_ab, creds_hl, creds_ma, creds_jb, creds_dp, creds_ez


urls_list = ['https://africacheck.info/3M1dMzf', 
             'https://africacheck.info/3B86Trx',
             'https://africacheck.info/3OqcoqI ',
             'https://africacheck.org/fact-checks/meta-programme-fact-checks/viral-video-showing-mugging-busy-street-guyana-not-kenya',
             'https://africacheck.org/fact-checks/reports/race-and-private-sector-ownership-south-africa-three-viral-claims-investigated',
            'https://africacheck.org/fact-checks/meta-programme-fact-checks/russian-company-accused-kenyan-government-selling-donated',
            'https://africacheck.info/3jpf2Cd ',
            'https://africacheck.org/fact-checks/meta-programme-fact-checks/no-bill-gates-has-not-vowed-pump-mrna-vaccines-food-supply',
            'https://africacheck.info/3W19A6K',
            'https://africacheck.org/fact-checks/meta-programme-fact-checks/south-africans-took-wrong-covid-vaccine-no-nasal-spray-drug',
            'https://africacheck.info/3RsXyBr',
            'https://africacheck.org/fact-checks/meta-programme-fact-checks/no-disturbing-video-attack-young-girl-was-not-shot-kenya',
            'https://africacheck.info/3jFojWA',
            'https://africacheck.org/fact-checks/spotchecks/data-doesnt-back-kenyan-transport-ministers-claim-road-crashes-have-claimed',
            'https://africacheck.info/fake_HIV_cure',
            'https://africacheck.org/fact-checks/reports/fraudulent-and-misleading-why-pricey-herbal-mix-touted-remedy-hiv-falls-flat',
            'https://africacheck.info/3WY6twP',
            'https://africacheck.org/fact-checks/meta-programme-fact-checks/ktn-news-reports-kenyan-doctor-danger-inventing-hypertension',
            'https://africacheck.info/3kGLAIe',
            'https://africacheck.org/fact-checks/meta-programme-fact-checks/no-hot-pineapple-water-wont-cure-cancer',
            'https://africacheck.org/fact-checks/meta-programme-fact-checks/no-kenyas-family-bank-not-offering-soft-loans-whatsapp',
            'https://africacheck.info/3lx7cHt',
            'https://africacheck.info/3wphJGV',
            'https://africacheck.org/fact-checks/meta-programme-fact-checks/no-weight-loss-rings-sold-facebook-just-another-scam-other',
            'https://africacheck.info/jobs_migrants_sa',
            'https://africacheck.org/fact-checks/reports/thousands-migrants-have-jobs-eskom-vodacom-and-south-africas-government-no',
            'https://africacheck.info/pfizer_employees',
            'https://africacheck.org/fact-checks/meta-programme-fact-checks/pfizer-employees-have-been-getting-vaccinated-against-covid',
            'https://africacheck.info/kenyatta_apology',
            'https://africacheck.org/fact-checks/meta-programme-fact-checks/former-kenyan-president-kenyatta-asking-forgiveness-new',
            'https://africacheck.info/british_ambassador_somalia',
            'https://africacheck.org/fact-checks/meta-programme-fact-checks/no-photo-doesnt-show-british-ambassador-somalia-black-child',
            'https://africacheck.info/kenya_exams',
            'https://africacheck.info/eskom_salaries',
            'https://africacheck.org/fact-checks/reports/no-70-kenyas-high-school-students-dont-score-d-or-lower',
            'https://africacheck.org/fact-checks/reports/overstaffing-and-billions-salaries-fact-checking-viral-claims-about-south',
            'https://africacheck.org/fact-checks/meta-programme-fact-checks/covid-vaccines-caused-huge-spike-medical-conditions-no-bad',
            'https://africacheck.info/3SlR1Yy',
            'https://africacheck.info/3U7Flt3',
            'https://africacheck.org/fact-checks/meta-programme-fact-checks/no-evidence-south-africas-minister-water-and-sanitation-said',
            'https://africacheck.info/kenya_volcano',
            'https://africacheck.org/fact-checks/meta-programme-fact-checks/no-kenyas-extinct-mount-longonot-didnt-erupt-fire-reported',
            'https://africacheck.info/onions_blood_pressure',
            'https://africacheck.org/fact-checks/meta-programme-fact-checks/no-eating-or-standing-onions-wont-cure-your-high-blood',
            'https://africacheck.info/youth_grant_scam',
            'https://africacheck.org/fact-checks/meta-programme-fact-checks/beware-fake-youth-empowerment-scheme-messages-whatsapp-and',
            'https://africacheck.org/fact-checks/meta-programme-fact-checks/no-kenyan-deputy-presidents-wife-not-giving-out-loans',
            'https://africacheck.info/gachagua_loan_scam',
            'https://www.dropbox.com/s/5gsfwz4tkv68q68/Africa_Check_message.mp4?dl=0',
            'https://www.dropbox.com/s/5gsfwz4tkv68q68/Africa_Check_youtube_message.mp4?dl=0',
            'https://www.dropbox.com/s/qdm1q17izyxiwyl/Africa_Check_vaccine.mp4?dl=0',
            'https://www.dropbox.com/s/kujk6fdjgasbkry/Africa_Check_youtube_image_220.mp4?dl=0',
            'https://www.dropbox.com/s/s702vzeb8sc39ij/Africa_Check_misinformation.mp4?dl=0',
            'https://www.dropbox.com/s/v8voe5fqa4b1fr2/Africa_Check_covid.mp4?dl=0',
            'https://www.dropbox.com/s/wl2mzgkvqnw47dl/Africa_Check_youtube_fakenews.mp4?dl=0',
            'https://www.dropbox.com/s/b2bxkbjwtzp2xnr/Africa_Check_health.mp4?dl=0',
            'https://www.dropbox.com/s/9gvyb2x4j09thiv/Africa_Check_facebook.mp4?dl=0']

words_tweets = ['speed and ease at which misinformation',  'Three viral claims are investigated here',
                'viral press statement', 'illustrates the increase in crime', 'understandable fears',
                'safety was not compromised', 'Covid vaccine misinformation', 'nasal spray to help treat Covid',
                'Pfizer employees employees are vaccinated', 'disturbing video of a girl being assaulted', 
                'fooled by misinformation', 'black child in a cage', 'much-criticised power utility',
                'verify images and videos on social', 'Get the facts here', 'get the facts here',
                'loved ones share misinformation', 'claims have been much shared online', 'much-criticised power utility',
                'people create health disinformation', 'brazen robbery and Kenyan social media',
                'protected by looking out for these hidden motivations','top tips for spotting false information',
                'Government ministers do sometimes put their feet in their mouths','Medical advice and health tips are everywhere', 'verification steps', 
                'fooled by using these simple steps', 'widespread antiretroviral treatment has drastically',
                'Kenyan doctor has invented a cure for high blood pressure', 'substances extracted from pineapples may help treat less serious conditions',
                'FactsMatter', 'factsmatter', 'Beware of posts offering soft loans on WhatsApp', 'too good to be true, it usually is',
                'rings sold with the promise of weight loss', 'through vaccinated livestock is pure fabrication',
               ]
