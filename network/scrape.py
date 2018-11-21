"""Experimenting with web scraping"""

from contextlib import closing
from requests import get
from requests.exceptions import RequestException
from bs4 import BeautifulSoup


def simple_get(url):
    """
    Tries to get the content at URL using a HTML request. Returns
    HTML/XML on success and None on failure
    """

    def is_good_response(resp):
        """
        Checks if a response is good.
        """
        content_type = resp.headers['Content-Type'].lower()
        return (resp.status_code == 200 and content_type is not None
                and content_type.find('html') > -1)

    def log_error(err):
        """
        Simple error logging wrapper
        """
        print(err)

    try:
        with closing(get(url, stream=True)) as resp:
            if is_good_response(resp):
                return resp.content
            return None

    except RequestException as err:
        log_error("Error during requests to {0} : {1}".format(url, str(err)))


sched = simple_get(
    "http://www3.rtd-denver.com/schedules/getSchedule.action?routeId=STMP")

html = BeautifulSoup(sched, 'html.parser')

stops = html.select('th[data-priority]')
times = html.select('tbody td')


def nth_list(html_list, cycle, n):
    return [item.contents[0] for item in html_list[n::cycle]]


all = []
for i in range(len(stops)):
    all.append(nth_list(times, len(stops), i))

dic = {stops[i].contents[0]: all[i] for i in range(len(stops))}
