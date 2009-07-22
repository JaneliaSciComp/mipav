import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.StreamTokenizer;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Scanner;
import java.util.Set;
import java.util.TreeMap;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.util.MipavUtil;
import gov.nih.mipav.view.Preferences;


public class PlugInAlgorithmCodebookParse extends AlgorithmBase {
	
	private File file = null;

	public PlugInAlgorithmCodebookParse(File selectedFiles) {
		file = selectedFiles;
	}


	public void runAlgorithm() {

		try {
			FileReader read = new FileReader(file);
			Scanner token = new Scanner(read).useDelimiter("[;|,|\n|\"]");
			TreeMap <String,Integer> holder = new TreeMap<String, Integer>();
			String current = null;
			
			
			


			
	      while (token.hasNext()) {
	    	  	
	        	  current = token.next().trim();

	        	  if(current.length() != 0){
		        	  if (holder.containsKey(current)){
		        		  holder.put(current, holder.get(current)+1);
		        	  }
		        	  else{
		        		  holder.put(current, 1);
		        	  }  
	        	  }
	          }

	      
	      read.close();
	      
	      Set set = holder.keySet();
	      String[] words = new String[set.size()];
	      int[] count = new int[set.size()];
	      int i = 0;
	      int j = set.size();
	      
	      Iterator runDown = set.iterator();
	      Preferences.data("Word Counts, alphabetical \n");
	      while (runDown.hasNext()){
	    	  words[i] = (String)runDown.next();
	    	  count[i] = holder.get(words[i]);
	    	  Preferences.data(words[i] + ":\t\t " + count[i] + " \n");
	    	  i++;
	      }
	      
	      Preferences.data("\n\nWord Counts, descending \n");
	      int max = -1;
    	  for (i =0; i < words.length; i++){
    		  if (count[i] > max){
    			  max = count[i];
    		  }  
    	  }
    	  
	      while (j != 0){
	    	  int nextmax = -1;
	    	  for (i =0; i < words.length;i++){
	    		  if (count[i] == max){
	    	    	  Preferences.data(words[i] + ":\t\t " + count[i] + " \n");
	    	    	  j--;
	    		  }
	    		  else if (count[i] > nextmax && count[i]<max){
	    			  nextmax = count[i];
	    		  }
	    	  }
	    	  max = nextmax;
	      }
	      
			
		
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		

	}

}
