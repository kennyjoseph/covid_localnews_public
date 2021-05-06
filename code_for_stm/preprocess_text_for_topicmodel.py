''''
Given a csv file formated for the STM, preprocess the text.

This is a strict preprocessing method to ensure we are left
with the most meaningful words in each document
''''

from nltk.corpus import stopwords
import gensim
stop_words = stopwords.words('english')

extended = ['from', 'subject', 're', 'edu', 'use', 'said', 'like', 'both', 'get', 'going', \
    'really', 'make', 'go', 'much', 'still', 'come', 'take', 'even', 'back', 'want', 'friday', \
    'monday', 'saturday', 'sunday', 'tuesday', 'thursday', 'week', 'afternoon', 'access', \
    'site', 'content', 'login', 'credentials', 'subscription', 'subscribe', 'please', 'read',\
    'free', 'article', 'local', 'none', 'que', 'los', 'del', 'con', 'las', 'por', 'una'\
    'time', 'think', 'one', 'thing', 'know', 'see', 'click', 'news', 'report', 'media', \
    'today', 'story', 'release', 'press', 'newspaper', 'stories']
stop_words.extend(extended)

def get_other_words_to_remove():
    words = []
    fns = ['state_names.txt', 'stop_words_spanish.txt']
    for fn in fns:
        with open(fn) as wd:
            for line in wd:
                w = line.strip().lower()
                words.append(w)
    stop_words.extend(words)


get_other_words_to_remove()

fn = "./Final_STM_on_alldata/stm_read_alldata_stringcov.csv"
outfn = "./Final_STM_on_alldata/stm_read_alldata_stringcov_preprocr2.csv"
na_found = 0

with open(outfn, "a") as out:
    with open(fn) as data:
        header = data.readline()
        out.write(header)
        for line in data:
            l = line.strip().split(",")
            c = l[-1]
            if c == 'NA':
                na_found+=1
                print("NA Found...")
                continue
            chars_to_replace = ["[", "]", ".", ",", "\"", "?", "!", ";", ":", "(", ")", "”"]
            for char in chars_to_replace:
                c = c.replace(char, "")
            c_preprocessed = gensim.utils.simple_preprocess(c)
            t = []
            for word in c_preprocessed:
                if word.isnumeric():
                    continue
                if word in stop_words:
                    continue
                t.append(word)
            newl = l[:-1]
            newl.append(str(" ".join(t)).lower())
            outl = ",".join(newl) +"\n"
            out.write(outl)
print("NAs found:", na_found)
