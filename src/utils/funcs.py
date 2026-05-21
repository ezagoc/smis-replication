#!python3.8

import pandas as pd
import numpy as np
import requests
import time

from pandas import json_normalize

deleted_influencers = ["shepherdmpofu", "brian_khisa"]

def get_influencers(country):
    """Twitter handles of participants"""
    base = f"../../data/01-characterize/influencers/{country}/"
    if country == "KE":
        file = "accounts_ke.xlsx"
    else:
        file = "accounts_sa.xlsx"
    path = f"{base}{file}"
    twitter_vars = pd.read_excel(path)
    base = f"../../data/02-randomize/{country}/00-participants/"
    file = "twitter_pilot_participants.xlsx"
    twitter_assignment = pd.read_excel(f"{base}{file}")
    twitter_assignment = twitter_assignment[~twitter_assignment['username'].isin(deleted_influencers)]
    twitter_assignment = twitter_assignment[twitter_assignment["country"]==country]
    twitter_vars = twitter_assignment.merge(twitter_vars, how='left', on="username")
    #twitter_vars.to_excel(f"../../data/02-randomize/{country}/02-variables/variables.xlsx")
    colskeep = ["username", "author_id", "name", "url"]
    twitter_vars = twitter_vars[colskeep]

    return twitter_vars

def get_participants_twitter(country):
    """Twitter handles of participants"""
    platforms = ["Facebook", "Twitter"]
    base = f"../../data/02-randomize/{country}/02-variables/"
    file = "variables_batch2.parquet"
    path = f"{base}{file}"
    twitter_vars = pd.read_parquet(path)
    base = f"../../data/02-randomize/{country}/03-assignment/output/"
    file = f"RandomizedTwitterSample{country}_batch2.xlsx"
    twitter_assignment = pd.read_excel(f"{base}{file}")
    twitter_assignment = twitter_assignment[["username", "treatment"]]
    twitter_vars = twitter_vars.merge(twitter_assignment, on="username")
    colskeep = ["username", "author_id", "name", "url", "treatment"]
    twitter_vars = twitter_vars[colskeep]

    return twitter_vars


def get_participants_facebook():
    """Facebook handles of participants"""
    platforms = ["Facebook", "Twitter"]
    base = "../../../data/02-randomize/first/02-variables/"
    file = "variables.xlsx"
    path = f"{base}{file}"
    vars0 = pd.read_excel(path, sheet_name=platforms)
    facebook_vars = vars0["Facebook"]
    base = "../../../data/02-randomize/first/03-assignment/output/"
    file = "RandomizedFacebookSample.xlsx"
    facebook_assignment = pd.read_excel(f"{base}{file}")
    facebook_assignment = facebook_assignment.iloc[:, 2:]
    facebook_vars = facebook_vars.merge(facebook_assignment, on="Page Url")
    colskeep = [
        "Facebook Id",
        "Page Name",
        "User Name",
        "Platform Id",
        "Page Url",
        "treatment",
    ]
    facebook_vars = facebook_vars[colskeep]

    return facebook_vars


class GetPosts:
    """For platformid it returns posts created during the time frame specified.
    Params:
    -----------
    ** account (str): Platform Id
    :param start (date):
    :param end_date
    Output:
    -----------
    ** df: Dataframe with all variables associated to the interaction the link
    had in Twitter.
    """

    def __init__(self, account, start_date, end_date, token):
        self.conn = "https://api.crowdtangle.com/posts?"
        self.account = account
        self.end_date = end_date
        self.token = token
        self.start_date = start_date

    def create_headers(self):
        headers = {}
        return headers

    def create_payload(self):
        payload = ""
        return payload

    def connect_to_endpoint(self, headers, params):
        time.sleep(1)
        while True:
            try:
                response = requests.request(
                    "GET", self.conn, headers=headers, params=params
                )
                print(response)
            except response.status_code != 200:
                time.sleep(10)
                continue
            break
        return response.json()

    def main(self):
        headers = self.create_headers()
        query_params = {
            "accounts": self.account,
            "count": 10000,
            "token": self.token,
            "startDate": self.start_date,
            "sortBy": "date",
            "endDate": self.end_date,
        }
        json_response = self.connect_to_endpoint(headers=headers, params=query_params)
        try:
            df = json_normalize(json_response["result"]["posts"])
            df = df.sort_index(axis=1)
            df.reset_index(drop=True, inplace=True)
            return df
        except:
            pass
            # df = df.append(pd.Series(), ignore_index=True)


class PreprocessTweets:
    def __init__(self, df):
        self.df = df
        self.column = "entities.urls"
        self.url = "expanded_url"
        self.text = "text"
        self.images = "images"

    def expand_column(self):

        df0 = self.df[~self.df[self.column].isna()]
        df0 = df0.reset_index(drop=True)
        dfi = pd.DataFrame(df0[self.column].tolist())
        dfi = dfi[0].apply(pd.Series)
        df0 = pd.concat([df0, dfi], axis=1)
        df0 = df0.reset_index(drop=True)
        df1 = self.df[self.df[self.column].isna()].reset_index(drop=True)
        df = pd.concat([df0, df1], ignore_index=True)
        df = df.reset_index(drop=True)

        return df

    def has_url(self, df):

        df["has_url"] = np.where(~df[self.url].isna(), 1, 0)

        return df

    def has_image(self, df):

        df["has_image"] = np.where(~df[self.images].isna(), 1, 0)

        return df

    def has_text(self, df):

        df["has_text"] = np.where(~df[self.text].isna(), 1, 0)

        return df

    def preprocess(self):

        df = self.expand_column()
        df = self.has_url(df)
        df = self.has_image(df)
        df = self.has_text(df)

        return df

new_inf = [
    "AshaJaffar",
    "reneengamau",
    "samkelemaseko",
    "XhantiPayi",
    "Masego_C",
    "MsNtuli",
    "DineshBalliah",
    "ArthiMtongana"]

