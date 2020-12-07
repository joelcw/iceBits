# -*- coding: utf-8 -*-

import csv
import argparse
import requests
import json
import re

from sys import stdout

character_sub_map = {
                     r'ã': 'a',
                     r'ä': 'a',
                     r'å': 'a',
                     r'ø': 'ö',
                     r'ü': 'u',
                     r'ė': 'é',
                     r'Ę': 'E',
                     r'ę': 'e',
                     r'ı': 'l',
                     r'Ń': 'N',
                     r'Ǫ': 'Ö',
                     r'ǫ': 'ö',
                     r'ǿ': 'ó',
                     r'ȧ': 'á',
                     r'ȯ': 'ó',
                     r'ɛ': 'e',
                     r'ʜ': 'h',
                     r'ο': 'o',
                     r'ẏ': 'ý',
                     # weird unicode characters
                     # u'': '',
                     # u'': '',
                     # u'': '',
                     # u'': '',
                     # u'': '',
                     # u'': '',
                     # u'': '',
                     # u'': '',
                     # u'': '',
                     # u'': '',
                     # u'': '',
                     # u'': '',
                     # u'': '',
                     # u'': '',
                     # u'': '',
                    }
unicode_chars = [ # for removing weird characters
                     'e499',
                     'e4c8',
                     'e4eb',
                     'e4ec',
                     'e60c',
                     'e715',
                     'e784',
                     'e8b4',
                     'ebce',
                     'ebdf',
                     'ebf5',
                     'ebf7',
                     'ebf9',
                     'ebfd',
                     'ebff',
                ]

wcat_map = {
            # tags from ABLtagger
            'k': 'k', # punctuation
            'm': 'm', # symbols
            'n': 'n', # nouns
            # word classes from MIM frequency overview (100+ instances)
            'adjective': 'l',
            'adverb': 'a',
            'article': 'g',
            'conjunction': 'c',
            'foreign word': 'e',
            'noun': 'n',
            'numeral': 't',
            'pronoun': 'f',
            'unanalysed word': 'x',
            'verb': 's',
            # tags from tei files
            '-': 'k',
            '.': 'k',
            '1': 'x',
            '\\': 'k',
            'a': 'a',
            'c': 'c',
            'e': 'e',
            'f': 'f',
            'g': 'g',
            'l': 'l',
            'nh': 'n',
            'nk': 'n',
            'nv': 'n',
            'nx': 'n',
            's': 's',
            't': 't',
            'x': 'x',
            '¨': 'm',
            '«': 'k',
            '¬': 'm',
            '°': 'm',
            '±': 'm',
            '²': 'm',
            '³': 'm',
            '´': 'm',
            '·': 'm',
            '»': 'k',
            '¼': 'm',
            '½': 'm',
            '–': 'k',
            '—': 'k',
            '‘': 'k',
            '’': 'k',
            '“': 'k',
            '”': 'k',
            '„': 'k',
            }
            

def standardize_lemma(lemma):
    """
    Changes lemma text used to find frequency
    """
    if lemma[0] in {'eg', 'vér', 'þér'}:
        return ('ég', lemma[1])
    else:
        return (lemma[0].lower(), lemma[1])

def decode_escaped(string, lemma=False):
    '''
    Fixes punctuations that are escaped in corpus data: -, / and '
    '''
    if re.search(r'[<>]', string):
        ''' Tokens processed '''
        if re.search(r'</?dash/?>', string):
            string= re.sub(r'</?dash/?>', '-',string)
        if re.search(r'</?slash/?>', string):
            string= re.sub(r'</?slash/?>', '/', string)
        if re.search(r'</?apostrophe/?>', string):
            string = re.sub(r'</?apostrophe/?>', "'", string)
        return string
    else:
        return string

def get_ts_output(file_handle):
    """
    Reads the Treebank Studio output tsv file and returns it as a list of lists
    """
    with open(file_handle, 'r') as file:
        # lines = file_handle.readlines()
        # for line in lines:
        #     print(line)
        ts_reader = csv.reader(file, delimiter='	')
        next(ts_reader)
        return list(ts_reader)
    
def lemmas_from_tagged(tagged_json):
    """
    Extracts a list of lemmas from the tagger output
    """
    lemmas = [(word[1][1], word[1][0][0]) for word in tagged_json]
    return lemmas

def tagged_sent(sent):
    """
    Calls tagging API from http://malvinnsla.arnastofnun.is/about_en
    """
    regex_map = re.compile("|".join(map(re.escape, character_sub_map.keys())))

    
    # unusual characters standardized
    sent = regex_map.sub(lambda match: character_sub_map[match.group(0)], sent)
    # weird unicode characters filtered out
    for char in sent:
        if '%04x' % ord(char) in unicode_chars:
            sent = sent.replace(char, '')
    # escaped puncutation decoded
    sent =  decode_escaped(sent)
    
    try:
        url = 'http://malvinnsla.arnastofnun.is'
        payload = {'text':sent, 'lemma':'on'}
        headers = {}
        res = requests.post(url, data=payload, headers=headers)
        tagged = json.loads(res.text)

        
        # return [(pair['word'],(pair['tag'],pair['lemma'])) for pair in tagged['paragraphs'][0]['sentences'][0]]
        output = []
        for p in tagged['paragraphs']:
            for s in p['sentences']:
                for pair in s:
                    output.append((pair['word'],(pair['tag'],pair['lemma'])))
        return output
    except:
        raise

def freq_dict(file_handle):
    """
    Saves the contents of lemma frequency tsv file as dict in the form (lemma, class): frequency
    """    
    print('Loading frequency file...')
    freqs = {}
    with open(file_handle, 'r') as file:
        lines = file.readlines()
        
        for line in lines:
            lemma, w_class, freq = line.strip('\n').split('\t')
            # add word + tag to frequency dict
            freqs[(lemma, wcat_map[w_class])] = int(freq) #/ 25000000
            # add only word to frequency dict
            freqs[lemma] = int(freq) #/ 25000000
    print('> Done')
    return freqs

def lemma_frequencies(lemma_list, all_lemma_freqs):
    """
    Finds frequency for each lemma in a sentence (lemma list) using frequency dict
    """
    freqs = []
    for lemma in lemma_list:
        # try block to look for word + tag and then only word if the frequency is 0
        try:
            # freqs.append(all_lemma_freqs.get(standardize_lemma(lemma), 0))
            freqs.append(all_lemma_freqs[standardize_lemma(lemma)])
        except KeyError:
            try: 
                freqs.append(all_lemma_freqs[standardize_lemma(lemma)[0]])
            except KeyError:
                freqs.append(all_lemma_freqs.get(standardize_lemma(lemma)[0], 0))
    return freqs

def write_output(file_handle, rows):
    """
    Writes output to new tsv file
    """
    with open(file_handle, 'w')  if file_handle else stdout  as file:
        # print('Writing to output:', file_handle)
        for row in rows:
            file.write('\t'.join(row)+'\n')
            
        print() # empty line after flush
        print('> Done')
        
        
if __name__ == '__main__':
    
    parser =  argparse.ArgumentParser(description='Script for adding lemma frequencies to Treebank Studio output tsv file')
    parser.add_argument('--input', '-i', required=True, help='path to input tsv file')
    parser.add_argument('--output', '-o', help='path to new tsv file')
    parser.add_argument('--lemma_list', '-l', required=True, help='path to file containing lemma frequencies')
    
    args = parser.parse_args()
    
    all_lemma_freqs = freq_dict(args.lemma_list)
    
    ts_file = get_ts_output(args.input)
    ts_length = len(ts_file)
    
    output_rows = []
    runner = 0
    
    print('Adding lemma frequency to tsv file...')
    for row in ts_file:
        runner += 1
        stdout.write(f'\r> {str(runner)} of {str(ts_length)}')
        stdout.flush()
        
        # # DEBUG: 
        # print()
        # print(row[-1])

        tagged = tagged_sent(row[-1])
        lemmas = lemmas_from_tagged(tagged)
        frequencies = lemma_frequencies(lemmas, all_lemma_freqs)
        
        # # DEBUG: 
        # for i in list(zip(lemmas, frequencies)):
        #     print(i)
        # input()
            
        new_row = row
        new_row.append(str(frequencies))
        output_rows.append(new_row)
    
    write_output(args.output, output_rows)
    
    # stdout.write('> Done')

        
        
