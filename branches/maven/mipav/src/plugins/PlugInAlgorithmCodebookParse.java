import gov.nih.mipav.model.algorithms.AlgorithmBase;

import gov.nih.mipav.view.Preferences;

import java.io.*;
import java.util.*;


public class PlugInAlgorithmCodebookParse extends AlgorithmBase {

    /* file to parse */
    private File file = null;

    public PlugInAlgorithmCodebookParse(final File selectedFiles) {
        file = selectedFiles;
    }

    public void runAlgorithm() {

        try {
            final FileReader read = new FileReader(file);
            final Scanner token = new Scanner(read).useDelimiter("[;|,|\n|\"]");

            // something to hold words and counts
            final TreeMap<String, Integer> holder = new TreeMap<String, Integer>();
            String current = null;

            while (token.hasNext()) { // while there are still words in the text

                current = token.next().trim();

                // if the word exists in holder add to count, otherwise add to list
                if (current.length() != 0) {
                    if (holder.containsKey(current)) {
                        holder.put(current, holder.get(current) + 1);
                    } else {
                        holder.put(current, 1);
                    }
                }
            }

            read.close();

            final Set<String> set = holder.keySet();
            final String[] words = new String[set.size()];
            final int[] count = new int[set.size()];
            int i = 0;
            int j = set.size();

            final Iterator<String> runDown = set.iterator();
            Preferences.data("Word Counts, alphabetical \n");

            // print out words alphabetically
            while (runDown.hasNext()) {
                words[i] = runDown.next();
                count[i] = holder.get(words[i]);
                Preferences.data(words[i] + ":\t\t " + count[i] + " \n");
                i++;
            }

            // print out words from top count to least
            Preferences.data("\n\nWord Counts, descending \n");
            int max = -1;
            for (i = 0; i < words.length; i++) {
                if (count[i] > max) {
                    max = count[i];
                }
            }

            while (j != 0) {
                int nextmax = -1;
                for (i = 0; i < words.length; i++) {
                    if (count[i] == max) {
                        Preferences.data(words[i] + ":\t\t " + count[i] + " \n");
                        j--;
                    } else if (count[i] > nextmax && count[i] < max) {
                        nextmax = count[i];
                    }
                }
                max = nextmax;
            }

        } catch (final FileNotFoundException e) {
            e.printStackTrace();
        } catch (final IOException e) {
            e.printStackTrace();
        }

    }

}
