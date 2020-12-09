replace_abb <- function(x){
  dict <- c('ADJ'= 'adjective',
            'ADP'= 'adposition',
            'ADV'= 'adverb',
            'AUX'= 'auxiliary',
            'CCONJ'= 'coordinating conjunction',
            'DET'= 'determiner',
            'INTJ'= 'interjection',
            'NOUN'= 'noun',
            'NUM'= 'numeral',
            'PART'= 'particle',
            'PRON'= 'pronoun',
            'PROPN'= 'proper noun',
            'PUNCT'= 'punctuation',
            'SCONJ'= 'subordinating conjunction',
            'SYM'= 'symbol',
            'VERB'= 'verb',
            'X'= 'other')
  x[,1] <- stringr::str_replace_all(string = x[,1],
                                       pattern= dict)
  return(x)
}