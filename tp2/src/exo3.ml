 let rec sentence_to_morse liste=
   match liste with
   |[]->[]
   |element::reste->word_to_morse element::sentence_to_morse reste;; 
 let rec sentence_to_single_list liste=
   match liste with
   |[]->[]
   |element::reste-> append (to_single_list element)  ('/'::sentence_to_single_list reste);;
 let to_single_morse liste=
   to_single_list(word_to_morse(liste));;
 let rec latin_sentence_to_single liste=
   match liste with
   |[]->[]
   |element::[]->to_single_morse(element)
   |element::reste->append (to_single_morse(element)) ('/'::latin_sentence_to_single(reste));;
