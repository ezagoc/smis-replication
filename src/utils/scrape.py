#!python3.8

import time
import requests
import pandas as pd

from pandas import json_normalize
from tqdm import tqdm
from tweetple import TweetPle


def scrape_posts(path, accounts, start_date, end_date, token):
    """Scrape Posts"""
    for account in tqdm(accounts):
        time.sleep(10)
        try:
            posts = GetPosts(
                account,
                start_date,
                end_date,
                token).main()
            posts.to_parquet(f'{path}/{account}.parquet')
        except ValueError:
            print(f"Oops!  Not content for {account}.")

    return print('Content scraped')


def scrape_tweets(accounts, bearer_token, path, start_date, end_date):
    """Scrape Tweets"""
    TweetPle.TweetStreamer(
        accounts,
        bearer_token,
        path,
        start_date,
        end_date
    ).main()

    return print('Content scraped')


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
        self.conn = 'https://api.crowdtangle.com/posts?'
        self.account = account
        self.end_date = end_date
        self.token = token
        self.start_date = start_date

    def create_headers(self):
        headers = {}
        return headers

    def create_payload(self):
        payload = ''
        return payload

    def connect_to_endpoint(self, headers, params):
        time.sleep(1)
        while True:
            try:
                response = requests.request(
                    "GET",
                    self.conn,
                    headers=headers,
                    params=params
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
            'accounts': self.account,
            'count': 10000,
            'token': self.token,
            "startDate": self.start_date,
            "sortBy": 'date',
            "endDate": self.end_date
        }
        json_response = self.connect_to_endpoint(
            headers=headers,
            params=query_params
        )
        try:
            df = json_normalize(json_response['result']['posts'])
            df = df.sort_index(axis=1)
            df.reset_index(drop=True,
                           inplace=True)
            return df
        except:
            pass
