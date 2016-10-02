# hoogle.py
# Hoogle queries
#
# Jonatan H Sundqvist
# October 2 2016

import requests
from bs4 import BeautifulSoup

template = 'https://www.stackage.org/{resolver}/hoogle?q={query}'


def request(resolver, query):
  path = template.format(resolver=resolver, query=query)
  # TODO: Does the get method take care of url encoding (?)
  response = requests.get(path)
  
  soup = BeautifulSoup(response.text, 'html.parser')

  for li in soup.find_all('ol', class_='search-results')[0].find_all('li'):
    print(type(li))


def main():
  request(resolver='lts-7.0', query='(a -> b) -> [a] -> [b]')


if __name__ == '__main__':
  main()