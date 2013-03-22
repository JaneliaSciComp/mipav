import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.view.MipavUtil;

/**
 * Plugin that parses and searches the IMFAR document
 * @author pandyan
 *
 */
public class PlugInAlgorithmIMFAR extends AlgorithmBase {

	/** path to imfar doc **/
	private String imfarDocPath, searchDocPath;

	/** array of search keywords **/
	private String[] searchFields;
	
	/** output filename **/
	private String outputDocPath;
	
	/** boolean to include conclusion in output **/
	private boolean includeConclusion;
	
	/** number of pre/post words to add on the n= search result **/
	private int numSubjectsPrePost;
	
	/** delimter in search results **/
	private String delimiter = "::";
	
	
	/**
	 * constructor
	 * @param imfarDocPath
	 */
	public PlugInAlgorithmIMFAR(String imfarDocPath, String searchDocPath, String outputDocPath, boolean includeConclusion, int numSubjectsPrePost) {
		this.imfarDocPath = imfarDocPath;
		this.searchDocPath = searchDocPath;
		this.outputDocPath = outputDocPath;
		this.includeConclusion = includeConclusion;
		this.numSubjectsPrePost = numSubjectsPrePost;
	}
	


	
	
	/**
	 * run algorithm
	 */
	public void runAlgorithm() {
		try{
			//parse search doc if user supplies it
			if(!searchDocPath.equals("")) {
				parseSearchDoc();
			}
			
			String line;
			FileInputStream fin = new FileInputStream(imfarDocPath);
			BufferedReader br = new BufferedReader(new InputStreamReader(fin));
			FileOutputStream fout = new FileOutputStream(outputDocPath);
			BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(fout));
			
			//column headers
			bw.write("Poster Section");
			bw.write("\t");
			bw.write("Abstract ID");
			bw.write("\t");
			bw.write("TITLE");
			bw.write("\t");
			bw.write("First Author");
			bw.write("\t");
			bw.write("First Author Affiliation");
			bw.write("\t");
			bw.write("Last Author");
			bw.write("\t");
			bw.write("Last Author Affiliation");
			bw.write("\t");
			bw.write("All Authors");
			bw.write("\t");
			if(searchFields != null && searchFields.length > 0) {
				for(int i=0;i<searchFields.length;i++) {
					bw.write(searchFields[i]);
					bw.write("\t");
				}
			}
			bw.write("n= ");
			bw.write("\t");
			if(includeConclusion) {
				bw.write("Abstract Conclusion");
			}
			bw.write("\r\n");
			
			
			//read in input doc line by line
			while((line = br.readLine()) != null) {
				if(line.trim().equals("104 Communication Posters 1") ||
						line.trim().equals("105 Human Genetics Posters 1") ||
						line.trim().equals("106 Clinical Phenotype Posters 1") ||
						line.trim().equals("107 Brain Imaging Posters 1") ||
						line.trim().equals("108 Sensory Systems Posters") ||
						line.trim().equals("114 Treatment Posters 1") ||
						line.trim().equals("115 Cognition Posters 1") ||
						line.trim().equals("116 Motor & Imitation Posters") ||
						line.trim().equals("117 Services Posters 1") ||
						line.trim().equals("118 Comorbidity Posters") ||
						line.trim().equals("123 Developmental Stages Posters") ||
						line.trim().equals("124 Human Genetics Posters 2") ||
						line.trim().equals("125 Social Function Posters 1") ||
						line.trim().equals("126 Epidemiology Posters") ||
						line.trim().equals("134 Treatment Posters 2") ||
						line.trim().equals("135 Communication Posters 2") ||
						line.trim().equals("136 Cognition Posters 2") ||
						line.trim().equals("137 Neurophysiology Posters") ||
						line.trim().equals("138 Repetitive Behaviour Posters") ||
						line.trim().equals("143 Services Posters 2") ||
						line.trim().equals("144 Brain Imaging Posters 2") ||
						line.trim().equals("145 Social Function Posters 2") ||
						line.trim().equals("146 Clinical Phenotype Posters 2") ||
						line.trim().equals("154 Language Posters") ||
						line.trim().equals("155 Neuropathology Posters") ||
						line.trim().equals("156 Cognition Posters 3") ||
						line.trim().equals("157 Cell/Animal Model Posters") ||
						line.trim().equals("158 Play Posters")) {
					
					String type = line.trim();
					String id = getID(line);
					int index = 1;
					if(line.trim().equals("106 Clinical Phenotype Posters 1")){
						//section 106 index starts at 2 in the document
						index = 2;
					}
					String nextSection = "Poster Presentations Program";
					if(line.trim().equals("158 Play Posters")){
						//section 158 finishes with the END keyword
						nextSection = "END";
					}
					
					
					while(!line.trim().equals(nextSection)) {
						line = br.readLine();
						String abstractId = id + "." + index;
						
						
						if(line.contains(abstractId)){
							//this represents the line with the abstract id, title, authors, and universities
							String abstactIDAndtitleAndAuthors = line.trim();
							String background = "";
							String objectives = "";
							String methods = "";
							String results = "";
							String conclusions = "";
							while(!(line = br.readLine()).startsWith("Background:")) {
								abstactIDAndtitleAndAuthors = abstactIDAndtitleAndAuthors + line.trim();
							}
							int begin = abstactIDAndtitleAndAuthors.indexOf(abstractId) + abstractId.length();
							String titleAndAuthors = (abstactIDAndtitleAndAuthors.substring(begin, abstactIDAndtitleAndAuthors.length())).trim();
							int firstPeriodIndex = titleAndAuthors.indexOf(".");
							String title = (titleAndAuthors.substring(0, firstPeriodIndex + 1)).trim();
							String authors = (titleAndAuthors.substring(firstPeriodIndex + 1, titleAndAuthors.length())).trim();
							String[] authsAndUnivs = parseAuthorsAndUnivs(authors);
							ArrayList<String> allAuthorsList = parseAllAuthors(authors);
							StringBuffer allAuthors = new StringBuffer();
							for(int i=0;i<allAuthorsList.size();i++) {
								allAuthors.append(allAuthorsList.get(i));
								if(allAuthorsList.size() > 1 && i != allAuthorsList.size() - 1) {
									allAuthors.append(" , ");
								}
							}
							
							//line is now at Background:
							while(!(line = br.readLine()).startsWith("Objectives:")) {
								background = background + line.trim();
							}
							while(!(line = br.readLine()).startsWith("Methods:")) {
								objectives = objectives + line.trim();
							}
							while(!(line = br.readLine()).startsWith("Results:")) {
								methods = methods + line.trim();
							}
							while(!(line = br.readLine()).startsWith("Conclusions:")) {
								results = results + line.trim();
							}
							while(!(line = br.readLine()).startsWith("###")) {
								conclusions = conclusions + line.trim();
							}
							
							//concatentate all sections to fullText
							String fullText = title + " " + background + " " + objectives + " " + methods + " " + results + " " + conclusions;
							fullText = fullText.toLowerCase();
							
							boolean writeOut = false;
							//we want to write out entries only if a search came back with yes
							if(searchFields != null && searchFields.length > 0) {
								for(int i=0;i<searchFields.length;i++) {
									String field = searchFields[i];
									if(searchFields[i].matches(".*:\\d+")) {
										field = searchFields[i].substring(0, searchFields[i].lastIndexOf(":"));
									}
									boolean containsSearch = containsSearch(fullText,field);
									if(containsSearch) {
										writeOut = true;
										break;
									}
								}
							}else {
								//no search to do....write out all entries
								writeOut = true;
							}
							

							if(writeOut) {
								//write out to file
								bw.write(type);
								bw.write("\t");
								bw.write(abstractId);
								bw.write("\t");
								bw.write(title);
								bw.write("\t");
								if(authsAndUnivs.length == 1) {
									//1 author
									String[] splits = authsAndUnivs[0].split(" : ");
									bw.write(splits[0]);
									bw.write("\t");
									bw.write(splits[1]);
									bw.write("\t");
									bw.write(" ");
									bw.write("\t");
									bw.write(" ");
									bw.write("\t");
									bw.write(allAuthors.toString());
									bw.write("\t");	
								}else {
									//at least 2 authors
									String[] splits1 = authsAndUnivs[0].split(" : ");
									bw.write(splits1[0]);
									bw.write("\t");
									bw.write(splits1[1]);
									bw.write("\t");
									String[] splits2 = authsAndUnivs[1].split(" : ");
									bw.write(splits2[0]);
									bw.write("\t");
									bw.write(splits2[1]);
									bw.write("\t");
									bw.write(allAuthors.toString());
									bw.write("\t");
									
								}
								
								//do searching based on keywords from search document user optionally supplies
								if(searchFields != null && searchFields.length > 0) {
									for(int i=0;i<searchFields.length;i++) {
										if(searchFields[i].matches(".*:\\d+")) {
											String field = searchFields[i];
											if(searchFields[i].matches(".*:\\d+")) {
												field = searchFields[i].substring(0, searchFields[i].lastIndexOf(":"));
											}
											boolean containsSearch = containsSearch(fullText,field);
											String result = "";
											if(containsSearch){
												result  = getResultString(fullText,searchFields[i]);
											}
											bw.write(result);
											bw.write("\t");
										}
										else {
											String field = searchFields[i];
											if(searchFields[i].matches(".*:\\d+")) {
												field = searchFields[i].substring(0, searchFields[i].lastIndexOf(":"));
											}
											boolean containsSearch = containsSearch(fullText,field);
											String containsSearchString;
											if(containsSearch) {
												containsSearchString = "1";
											}else {
												containsSearchString = "0";
											}
											bw.write(containsSearchString);
											bw.write("\t");
										}
									}
								}
								
								//search on n= to get the number
								String numberSubjectsString = getNumberSubjects(fullText);
								bw.write(numberSubjectsString);
								bw.write("\t");
								
								if(includeConclusion) {
									bw.write(conclusions);
								}
								bw.write("\r\n");
							}

							++index;
							
						}
					}

				}

			}
			br.close();
			fin.close();
			bw.close();
			fout.close();
		}catch(FileNotFoundException e) {
			setCompleted(true);
			MipavUtil.displayError("FileNotFoundException");
			e.printStackTrace();
			return;
			
		}catch(IOException e) {
			setCompleted(true);
			MipavUtil.displayError("IOException");
			e.printStackTrace();
			return;
		}

		setCompleted(true);
	}
	
	
	/**
	 * returns the ID
	 * @param line
	 * @return
	 */
	public String getID(String line) {
		if(line.trim().equals("104 Communication Posters 1")){
			return "104";
		}
		else if(line.trim().equals("105 Human Genetics Posters 1")){
			return "105";
		}
		else if(line.trim().equals("106 Clinical Phenotype Posters 1")){
			return "106";
		}
		else if(line.trim().equals("107 Brain Imaging Posters 1")){
			return "107";
		}
		else if(line.trim().equals("108 Sensory Systems Posters")){
			return "108";
		}
		else if(line.trim().equals("114 Treatment Posters 1")){
			return "114";
		}
		else if(line.trim().equals("115 Cognition Posters 1")){
			return "115";
		}
		else if(line.trim().equals("116 Motor & Imitation Posters")){
			return "116";
		}
		else if(line.trim().equals("117 Services Posters 1")){
			return "117";
		}
		else if(line.trim().equals("118 Comorbidity Posters")){
			return "118";
		}
		else if(line.trim().equals("123 Developmental Stages Posters")){
			return "123";
		}
		else if(line.trim().equals("124 Human Genetics Posters 2")){
			return "124";
		}
		else if(line.trim().equals("125 Social Function Posters 1")){
			return "125";
		}
		else if(line.trim().equals("126 Epidemiology Posters")){
			return "126";
		}
		else if(line.trim().equals("134 Treatment Posters 2")){
			return "134";
		}
		else if(line.trim().equals("135 Communication Posters 2")){
			return "135";
		}
		else if(line.trim().equals("136 Cognition Posters 2")){
			return "136";
		}
		else if(line.trim().equals("137 Neurophysiology Posters")){
			return "137";
		}
		else if(line.trim().equals("138 Repetitive Behaviour Posters")){
			return "138";
		}
		else if(line.trim().equals("143 Services Posters 2")){
			return "143";
		}
		else if(line.trim().equals("144 Brain Imaging Posters 2")){
			return "144";
		}
		else if(line.trim().equals("145 Social Function Posters 2")){
			return "145";
		}
		else if(line.trim().equals("146 Clinical Phenotype Posters 2")){
			return "146";
		}
		else if(line.trim().equals("154 Language Posters")){
			return "154";
		}
		else if(line.trim().equals("155 Neuropathology Posters")){
			return "155";
		}
		else if(line.trim().equals("156 Cognition Posters 3")){
			return "156";
		}
		else if(line.trim().equals("157 Cell/Animal Model Posters")){
			return "157";
		}
		else if(line.trim().equals("158 Play Posters")){
			return "158";
		}
		else{
			return "";
		}
		
	}
	
	/**
	 * does the search on the fullText for keywords
	 * @param fullText
	 * @param line
	 * @return
	 */
	public boolean containsSearch(String fullText, String line) {
		boolean containsSearch = false;
		//line might contain multiple words separated by spaces...if this is the case, we must search using "AND" for all the words
		String[] keyWords = line.split(",");
		if(keyWords.length > 1) {
			for(int j=0;j<keyWords.length;j++) {
				String word = keyWords[j].trim().toLowerCase();
				//phrases will be in quotes
				if(word.startsWith("\"")) {
					word = word.substring(1, word.length() - 1);
				}
				containsSearch = fullText.contains(word);
				//since we are ANDing all of them...if at least 1 is false, break out
				if(!containsSearch) {
					break;
				}
			}
		}else {
			String word = keyWords[0].toLowerCase();
			if(word.startsWith("\"")) {
				word = word.substring(1, word.length() - 1);
			}
			containsSearch = fullText.contains(word);
		}

		return containsSearch;
	}
	
	
	
	public String getResultString(String fullText, String line) {
		// get the number from :number
		String numString = line.substring(line.lastIndexOf(":")+1, line.length());
		int num = new Integer(numString).intValue();
		num = num - 1;
		//strip away the :number at end
		line = line.substring(0, line.lastIndexOf(":"));
		
		String result = "";
		String[] keyWords = line.split(",");

		for(int j=0;j<keyWords.length;j++) {
			String word = keyWords[j].trim().toLowerCase();
			
			//phrases will be in quotes
			if(word.startsWith("\"")) {
				word = word.substring(1, word.length() - 1);
			}
			//user might have entered 0...in that case bum woube be -1
			Pattern p;
			if(num == -1) {
				p = Pattern.compile(word);
			}else {
				p = Pattern.compile("([^\\s]+\\s){0," + num + "}([^\\s]+\\s?)?" + word + "(\\s?[^\\s]+)?(\\s[^\\s]+){0," + num + "}");
			}
			Matcher m = p.matcher(fullText);
			int i = 0;
			while(m.find()) {
				String s = m.group();
				if(i == 0) {
					result = result + s;
				}else {
					result = result + delimiter + s;
				}
				i++;
			}
			
			result = result + delimiter;
		}

		result = result.substring(0, result.lastIndexOf(delimiter));
	
		return result;
	}
	
	/**
	 * gets number of subjects by searching for n=
	 * @param fullText
	 * @return
	 */
	public String getNumberSubjects(String fullText) {
		int i=0;
		String finalString = "";
		String s1 = "";
		int num = numSubjectsPrePost - 1;
		//user might have entered 0...in that case bum woube be -1
		Pattern p;
		if(num == -1) {
			p = Pattern.compile("[^a-z^A-Z][nN]\\s*?=\\s*?\\d+");
		}else {
			p = Pattern.compile("([^\\s]+\\s){0," + num + "}([^\\s]+\\s?)?[^a-z^A-Z][nN]\\s*?=\\s*?\\d+(\\s?[^\\s]+)?(\\s[^\\s]+){0," + num + "}");
		}
		
		
		
		
		Matcher m = p.matcher(fullText);
		
		while(m.find()) {
			s1 = m.group();
			if(num == -1) {
				s1 = s1.substring(1, s1.length());
			}
			if(i==0) {
				finalString = s1;
			}else {
				finalString = finalString + delimiter + s1;
			}
			
			i++;

		}
		
		return finalString;
	}
	
	/***
	 * parses the search document to extract the keyword lines
	 */
	public void parseSearchDoc() {
		try {
			FileInputStream fin = new FileInputStream(searchDocPath);
			BufferedReader br = new BufferedReader(new InputStreamReader(fin));
			String line;
			int numRows = 0;
			while((line = br.readLine()) != null) {
				if(!line.trim().equals("")){
					numRows++;
				}
			}
			if(numRows > 0) {
				searchFields = new String[numRows];
			}
			fin.close();
			br.close();
			fin = new FileInputStream(searchDocPath);
			br = new BufferedReader(new InputStreamReader(fin));
			int i = 0;
			while((line = br.readLine()) != null) {
				if(!line.trim().equals("")){
					searchFields[i] = line;
					i++;
				}
			}
		}catch(FileNotFoundException e) {
			
		}catch(IOException e) {
			
		}
		
	}
	
	/**
	 * parse out first author, last author and universities
	 * @param authorsAndUnivs
	 * @return
	 */
	public String[] parseAuthorsAndUnivs(String authorsAndUnivs) {

	String[] splits = authorsAndUnivs.split(",");
	//remove * and spaces
	for(int i=0;i<splits.length;i++) {
		splits[i] = splits[i].replaceAll("\\*", "");
		splits[i] = splits[i].trim();
	}

	boolean containsMultipleUniversities = false;
	ArrayList<String> universities = null;
	int index = -1;
	for(int i=0;i<splits.length;i++) {
		if(splits[i].startsWith("(")) {
			containsMultipleUniversities = true;
			index = i;
			break;
		}
		
	}
	if(!containsMultipleUniversities) {
		if(splits.length == 2) {
			if(splits[0].contains(" and")){
				String[] authsSplit = splits[0].split(" and");
				for(int i=0;i<authsSplit.length;i++) {
					authsSplit[i] = authsSplit[i].trim();
				}
				//strip away first initials
				
				authsSplit[0] = authsSplit[0].replaceAll("[a-zA-Z]\\. ", "") + " : " + splits[1];
				authsSplit[1] = authsSplit[1].replaceAll("[a-zA-Z]\\. ", "") + " : " + splits[1];
				String[] splits2 = {authsSplit[0], authsSplit[1]};
				return splits2;
			}else {
				//strip away first initials
				splits[0] = splits[0].replaceAll("[a-zA-Z]\\. ", "") + " : " + splits[1];
				String[] splits2 = {splits[0]};
				return splits2;
			}
		}else {
			if(splits[splits.length - 2].contains(" and")){
				String[] authsSplit = splits[splits.length - 2].split(" and");
				for(int i=0;i<authsSplit.length;i++) {
					authsSplit[i] = authsSplit[i].trim();
				}
				//strip away first initials
				splits[0] = splits[0].replaceAll("[a-zA-Z]\\. ", "") + " : " + splits[splits.length - 1];
				authsSplit[1] = authsSplit[1].replaceAll("[a-zA-Z]\\. ", "") + " : " + splits[splits.length - 1];
				String[] splits2 = {splits[0], authsSplit[1]};
				return splits2;
			}
		}
	}else {//contains multiple universities
		if(splits.length == 3) {
			if(splits[0].contains(" and")){
				String[] authsSplit = splits[0].split(" and");
				for(int i=0;i<authsSplit.length;i++) {
					authsSplit[i] = authsSplit[i].trim();
				}
				universities = new ArrayList<String>();
				if(splits[1].startsWith("(")) {
					String univ1 = splits[1].replaceAll("\\([1-9]\\)","");
					if(univ1.equals("")) {
						univ1 = "NOT AVAILABLE";
					}
					universities.add(univ1);
				}
				if(splits[2].startsWith("(")) {
					String univ2 = splits[2].replaceAll("\\([1-9]\\)","");
					if(univ2.equals("")) {
						univ2 = "NOT AVAILABLE";
					}
					universities.add(univ2);
				}

				//strip away first initials and get university affiliation number
				authsSplit[0] = authsSplit[0].replaceAll("[a-zA-Z]\\. ", "");
				String affiliationString = authsSplit[0].substring(authsSplit[0].length() - 1);
				int affiliationNumber = Integer.valueOf(affiliationString);
				authsSplit[0] = authsSplit[0].substring(0, authsSplit[0].length() - 1) + " : " + universities.get(affiliationNumber - 1);

				authsSplit[1] = authsSplit[1].replaceAll("[a-zA-Z]\\. ", "");
				affiliationString = authsSplit[1].substring(authsSplit[1].length() - 1);
				affiliationNumber = Integer.valueOf(affiliationString);
				authsSplit[1] = authsSplit[1].substring(0, authsSplit[1].length() - 1) + " : " + universities.get(affiliationNumber - 1);
				
				String[] splits2 = {authsSplit[0], authsSplit[1]};
				return splits2;
			}
			
		}else {
			if(splits[index - 1].contains(" and")) {
				String[] authsSplit = splits[index - 1].split(" and");
				for(int i=0;i<authsSplit.length;i++) {
					authsSplit[i] = authsSplit[i].trim();
				}
				universities = new ArrayList<String>();
				for(int i=index,k=0;i<splits.length;i++) {
					if(splits[i].startsWith("(")) {
						String univ1 = splits[i].replaceAll("\\(\\d{1,2}?\\)","");
						if(univ1.equals("")) {
							univ1 = "NOT AVAILABLE";
						}
						universities.add(univ1);
						k++;
					}else {
						String univString = universities.get(k-1);
						univString = univString + " " + splits[i];
						universities.add(k-1, univString);
						
					}
				}
	
				//strip away first initials
				splits[0] = splits[0].replaceAll("[a-zA-Z]\\. ", "");
				String affiliationString = splits[0].substring(splits[0].length() - 1);
				int affiliationNumber = Integer.valueOf(affiliationString);
				splits[0] = splits[0].substring(0, splits[0].length() - 1) + " : " + universities.get(affiliationNumber - 1);
				
				
				authsSplit[1] = authsSplit[1].replaceAll("[a-zA-Z]\\. ", "");
				affiliationString = authsSplit[1].substring(authsSplit[1].length() - 1);
				affiliationNumber = Integer.valueOf(affiliationString);
				String affiliationString2;
				affiliationString2 = authsSplit[1].substring(authsSplit[1].length() - 2);
			
				boolean doubleDigit = false;
				try{
					Integer.valueOf(affiliationString);
					affiliationNumber = Integer.valueOf(affiliationString + affiliationString2);
					doubleDigit = true;
					
				}catch(NumberFormatException e) {
					
				}
				if(doubleDigit) {
					authsSplit[1] = authsSplit[1].substring(0, authsSplit[1].length() - 2) + " : " + universities.get(affiliationNumber - 1);
				}else {
					authsSplit[1] = authsSplit[1].substring(0, authsSplit[1].length() - 1) + " : " + universities.get(affiliationNumber - 1);
				}
				
				
				String[] splits2 = {splits[0], authsSplit[1]};
				return splits2;
			}
			
		}
		
		
	}

	return splits;
	}


	/**
	 * parse out all authors
	 * @param authorsAndUnivs
	 * @return
	 */
	public ArrayList<String> parseAllAuthors(String authorsAndUnivs) {
	ArrayList<String> authorsList = new ArrayList<String>();
	String affiliationString;

	boolean doubleDigit;
	String[] splits = authorsAndUnivs.split(",");
	//remove * and spaces
	for(int i=0;i<splits.length;i++) {
		splits[i] = splits[i].replaceAll("\\*", "");
		splits[i] = splits[i].trim();
	}
	

	boolean containsMultipleUniversities = false;
	int index = -1;
	for(int i=0;i<splits.length;i++) {
		if(splits[i].startsWith("(")) {
			containsMultipleUniversities = true;
			index = i;
			break;
		}
		
	}
	
	if(!containsMultipleUniversities) {
		if(splits.length == 2) {
			if(splits[0].contains(" and")){
				String[] authsSplit = splits[0].split(" and");
				for(int i=0;i<authsSplit.length;i++) {
					authsSplit[i] = authsSplit[i].trim();
				}
				//strip away first initials...commenting out 10/15/08
				//authsSplit[0] = authsSplit[0].replaceAll("[a-zA-Z]\\. ", "");
				authorsList.add(authsSplit[0]);
				//authsSplit[1] = authsSplit[1].replaceAll("[a-zA-Z]\\. ", "");
				authorsList.add(authsSplit[1]);
				return authorsList;

			}else {
				//strip away first initials...commenting out 10/15/08
				//splits[0] = splits[0].replaceAll("[a-zA-Z]\\. ", "");
				authorsList.add(splits[0] );
				return authorsList;
			}
		}else {
			if(splits[splits.length - 2].contains(" and")){
				String[] authsSplit = splits[splits.length - 2].split(" and");
				for(int i=0;i<authsSplit.length;i++) {
					authsSplit[i] = authsSplit[i].trim();
				}
				//strip away first initials...commenting out 10/15/08

				for(int i=0;i<splits.length - 2;i++) {
					//splits[i] = splits[i].replaceAll("[a-zA-Z]\\. ", "");
					authorsList.add(splits[i]);
				}
				//authsSplit[0] = authsSplit[0].replaceAll("[a-zA-Z]\\. ", "");
				authorsList.add(authsSplit[0]);
				//authsSplit[1] = authsSplit[1].replaceAll("[a-zA-Z]\\. ", "");
				authorsList.add(authsSplit[1]);

				return authorsList;
			}
		}
	}else {//multiple universities
		
		if(splits[index - 1].contains(" and")) {
			String[] authsSplit = splits[index - 1].split(" and");
			for(int i=0;i<authsSplit.length;i++) {
				authsSplit[i] = authsSplit[i].trim();
				//authsSplit[i].replaceAll("[a-zA-Z]\\. ", "");
			}
			
			for(int i=0;i<index-1;i++) {
		
				//splits[i] = splits[i].replaceAll("[a-zA-Z]\\. ", "");
				affiliationString = splits[i].substring(splits[i].length() - 1);
				try{
					Integer.valueOf(affiliationString);
				}catch(NumberFormatException e) {
					
				}
				String affiliationString2;
				affiliationString2 = splits[i].substring(splits[i].length() - 2);
				doubleDigit = false;
				try{
					Integer.valueOf(affiliationString);
					Integer.valueOf(affiliationString + affiliationString2);
					doubleDigit = true;
					
				}catch(NumberFormatException e) {
					//do nothing
				}
				if(doubleDigit) {
					splits[i] = splits[i].substring(0, splits[i].length() - 2);
				}else {
					splits[i] = splits[i].substring(0, splits[i].length() - 1);
				}
				authorsList.add(splits[i]);
			}
			
			//authsSplit[0] = authsSplit[0].replaceAll("[a-zA-Z]\\. ", "");
			affiliationString = authsSplit[0].substring(authsSplit[0].length() - 1);
			
			boolean hasAffiliation = true;
			try{
				Integer.valueOf(affiliationString);
			}catch(NumberFormatException e) {
				hasAffiliation = false;
			}
			String affiliationString2;
			affiliationString2 = authsSplit[0].substring(authsSplit[0].length() - 2);
			doubleDigit = false;
			try{
				Integer.valueOf(affiliationString);
				Integer.valueOf(affiliationString + affiliationString2);
				doubleDigit = true;
				
			}catch(NumberFormatException e) {
				//do nothing
			}
			if(hasAffiliation) {
				if(doubleDigit) {
					authsSplit[0] = authsSplit[0].substring(0, authsSplit[0].length() - 2);
				}else {
					authsSplit[0] = authsSplit[0].substring(0, authsSplit[0].length() - 1);
				}
			}
			authorsList.add(authsSplit[0]);
			
			//authsSplit[1] = authsSplit[1].replaceAll("[a-zA-Z]\\. ", "");
			affiliationString = authsSplit[1].substring(authsSplit[1].length() - 1);
			Integer.valueOf(affiliationString);
			affiliationString2 = authsSplit[1].substring(authsSplit[1].length() - 2);
			doubleDigit = false;
			try{
				Integer.valueOf(affiliationString);
				Integer.valueOf(affiliationString + affiliationString2);
				doubleDigit = true;
				
			}catch(NumberFormatException e) {
				//do nothing
			}
			if(doubleDigit) {
				authsSplit[1] = authsSplit[1].substring(0, authsSplit[1].length() - 2);
			}else {
				authsSplit[1] = authsSplit[1].substring(0, authsSplit[1].length() - 1);
			}
			
			
			authorsList.add(authsSplit[1]);
			
			return authorsList;
			
		}
		
	}
		
		
	
	return authorsList;
}








}
