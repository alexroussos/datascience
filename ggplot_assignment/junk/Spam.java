import java.io.File;
import java.io.FileNotFoundException;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;
import java.util.TreeMap;


/**
 * 
 * 16 June 2013 for GADS4
 * 
 * Compute the spamminess metric for different words and output in csv [word,spamCount,hamCount,relFreq]
 * 
 * @author aroussos
 * 
 */
public class Spam {

    private static int MIN_SPAM_COUNT = 10;
    private static int MIN_HAM_COUNT = 10; // ONLY used when finding words in ham map that aren't in spam map
    private static int MIN_CHARS = 2;

    public static void main(String[] args) {
        Spam main = new Spam();
        String filename = "SMSSpamCollection";
        String path = "/Users/aroussos/Downloads/smsspamcollection/";
        File file = new File(path + filename);

        Map<String, Integer> spamMap = new HashMap<String, Integer>();
        Map<String, Integer> hamMap = new HashMap<String, Integer>();

        // Parse SMSes into unsorted maps with counts
        try {
            int numLines = 0;
            Scanner scanner = new Scanner(file);
            while (scanner.hasNextLine()) {
                String sms = scanner.nextLine();
                processSms(sms, spamMap, hamMap);
                numLines++;
            }
        } catch (FileNotFoundException e) {
            System.out.println(e);
        }

        // Sort the spam map so we can examine the top entries
        IntegerValueComparator spamComparator = main.new IntegerValueComparator(spamMap);
        TreeMap<String, Integer> sortedSpam = new TreeMap<String, Integer>(spamComparator);
        sortedSpam.putAll(spamMap);
        
        Map<String, Double> ratioMap = new HashMap<String, Double>();

        int totalSpamWords = spamMap.size();
        int totalHamWords = hamMap.size();

        // For each spam word, find the relative frequency of it vs the same word in non-spam and add thay ratio to a map
        for (Map.Entry<String, Integer> spamWord : sortedSpam.entrySet()) {
            if (spamWord.getValue() < MIN_SPAM_COUNT) {
                break;
            }
            Integer spamWordCount = spamWord.getValue();
            Integer hamWordCount = hamMap.get(spamWord.getKey());
            double ratio = 0;
            if (hamWordCount != null && hamWordCount > 0) {
                ratio = (spamWordCount / (double) totalSpamWords) / (hamWordCount / (double) totalHamWords);
            } else {
                ratio = Integer.MAX_VALUE;
                hamMap.put(spamWord.getKey(), 0);
            }
            ratioMap.put(spamWord.getKey(), ratio);
        }

        // Go through the ham set and find any words that didn't appear in the spam set
        for (Map.Entry<String, Integer> hamWord : hamMap.entrySet()) {
            if (!spamMap.containsKey(hamWord) && hamWord.getValue() > MIN_HAM_COUNT) {
                spamMap.put(hamWord.getKey(), 0);
                ratioMap.put(hamWord.getKey(), 0.0);
            }
        }

        // Sort by spamminess of the word
        DoubleValueComparator ratioComparator = main.new DoubleValueComparator(ratioMap);
        TreeMap<String, Double> sortedRatios = new TreeMap<String, Double>(ratioComparator);
        sortedRatios.putAll(ratioMap);

        System.out.println("word,spamCount,hamCount,relFreq");
        for (String key : sortedRatios.keySet()) {
            System.out.println(key + "," + spamMap.get(key) + "," + hamMap.get(key) + "," + ratioMap.get(key));
        }
    }

    private static void processSms(String sms, Map<String, Integer> spamMap, Map<String, Integer> hamMap) {
        Map<String, Integer> map = null;
        if (isSpam(sms)) {
            map = spamMap;
            sms = sms.substring(5); // remove the first word, always 'spam '
        } else {
            map = hamMap;
            sms = sms.substring(4);
        }

        String[] words = tokenizeSms(sms);
        for (String word : words) {
            if (word != null) {
                if (word.length() >= MIN_CHARS) {
                    word = normalizeWord(word);
                    Integer count = map.get(word);
                    if (count == null) {
                        count = new Integer(0);
                    }
                    count++;
                    map.put(word, count);
                }
            }
        }
    }

    private static String[] tokenizeSms(String sms) {
        String[] words = sms.split(" ");
        return words;
    }

    private static String normalizeWord(String word) {
        String normalized = word.toLowerCase();
        normalized = normalized.replace(".", "").replace(",", "").replace("?", "").replace("!", "").replace(":", "");
        return normalized;
    }

    private static boolean isSpam(String sms) {
        return sms.startsWith("spam");
    }

    private class IntegerValueComparator implements Comparator<String> {

        Map<String, Integer> base;

        public IntegerValueComparator(Map<String, Integer> base) {
            this.base = base;
        }

        // Note: this comparator imposes orderings that are inconsistent with equals.
        public int compare(String a, String b) {
            if (base.get(a) >= base.get(b)) {
                return -1;
            } else {
                return 1;
            } // returning 0 would merge keys
        }
    }

    private class DoubleValueComparator implements Comparator<String> {

        Map<String, Double> base;

        public DoubleValueComparator(Map<String, Double> base) {
            this.base = base;
        }

        // Note: this comparator imposes orderings that are inconsistent with equals.
        public int compare(String a, String b) {
            if (base.get(a) >= base.get(b)) {
                return -1;
            } else {
                return 1;
            } // returning 0 would merge keys
        }
    }

}
