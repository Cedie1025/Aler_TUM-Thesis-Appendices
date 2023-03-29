import requests
from bs4 import BeautifulSoup
import csv
import unicodedata

# 1 - PARSE CONTENT FROM ALL INTERVIEWS
# as a work around for infinite scrolling, the HTML file has been downloaded and is being used to scrape
# the general data of the startups
data = open('interviews.html','r')
soup = BeautifulSoup(data, 'html.parser')

# 2 - ITERATE ALL LINkS FROM INTERVIEWS
interviews = soup.find_all('div', class_='w-dyn-item')

links = []
for interview in interviews:
    links.append(interview.find('a')['href'])
    interview.find('a')['href']

# 3 - ITERATE EACH LINKS FROM ALL INTERVIEWS
content = []
for link in links:

    questions = []
    answers = []
    URL = link
    startup = link.split('/')[4]
    website = requests.get(URL)
    results = BeautifulSoup(website.content, 'html.parser')

# 4 - SCRAPE GENERAL INFO FROM EACH INTERVIEWS
    name = results.find('div', class_='text-block-interview-name').text
    date = results.find('div', class_='text-block-interview-date').text
    title = results.find('h1', class_='content-h1').text
    summary = results.find('p', class_='content-summary').text
    tag = results.find_all('div', class_='text-block-interview-tags')
    category = tag[0].text
    country = tag[1].text
    cause_of_failure = tag[5].text
    revenue = tag[2].text

# 5 - SCRAPE ALL THE QUESTION AND ANSWERS FROM EACH INTERVIEWS

    # clean up - delete figures and links and linebreaks
    for figure in results.find_all('figure'): 
        figure.decompose()
    for link in results.find_all('a'):
        link.replace_with(link.text)
    for linebreak in results.find_all('br'):
        linebreak.decompose()
    text_blocks = results.find_all('div', class_="content-rich-text w-richtext")

    for block in text_blocks: 
        # get all questions in the block (normally three)
        questions_from_block = block.find_all('h2')
        for question in questions_from_block: 
            # for each question, append the question to the questions array
            questions.append(question.text)
            # go through sibling tags (i.e. p tags) of the question until the next h2-tag appears
            answer = ""
            for answer_part in question.next_siblings:
                if "h2" in answer_part.name:
                    break
                answer = answer + answer_part.text
            # append answer to answer array
            answers.append(answer)       

    for i in range(len(questions)):
        content.append({
                "Link": URL,
                "Founder's Name": name,
                "Date of Interview": date,
                "Category": category,
                "Country": country,
                "Cause of Failure": cause_of_failure,
                "Revenue": revenue,
                "Ttile": title,
                "Summary": summary,
                "Questions:": questions[i],
                "Answers:": answers[i]
        
        })
    
# 6 - WRITE TO CSV
    keys = content[0].keys()
  
    with open('SF_Narratives.csv', 'w', newline='') as output_file:
        dict_writer = csv.DictWriter(output_file, keys)
        dict_writer.writeheader()
        dict_writer.writerows(content)
