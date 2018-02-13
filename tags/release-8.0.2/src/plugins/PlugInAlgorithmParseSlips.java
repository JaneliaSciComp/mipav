import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;
import java.util.Vector;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.view.MipavUtil;



public class PlugInAlgorithmParseSlips extends AlgorithmBase{
	
	/**
	 * Lines from the samples report, contains the entire line
	 */
	private Vector<String[]> reportLines;
	
	/**
	 * Lines from the Coriell report, contains the entire line
	 */
	private Vector<String[]> slipLines;
	
	/**
	 * IDs from the samples report
	 */
	private Vector<String> reportID;
	
	/**
	 * IDs from the Coriell report
	 */
	private Vector<String> slipID;
	
	private File reportFile;
	
	private File slipFile;
	
	/**
	 * Output CSV specifying any differences occuring between
	 * the samples report and the Coriell report
	 */
	private FileWriter csv;
	
	/**
	 * Output CSV that appends the samples report info with the
	 * Coriell report info
	 */
	private FileWriter concatCSV;
	
	/**
	 * Used to trip break statements or to end the algorithm.
	 * Kind of messy, but it works.
	 */
	private boolean failed;
	
	/**
	 * Indecies of the pertinent columns in the Coriell file because
	 * for some reason they like to keep changing how the columns are
	 * organized and it keeps breaking the program
	 */
	private int[] indecies;
	
	public PlugInAlgorithmParseSlips(){
		super();
	}
	public PlugInAlgorithmParseSlips(File report, File slip, FileWriter csv, FileWriter concat){
		super();
		reportFile = report;
		slipFile = slip;
		this.csv = csv;
		concatCSV = concat;
		indecies = new int[11];
		Arrays.fill(indecies, -1);
	}
	
	@Override
	public void runAlgorithm() {
		int index;
		String reportIDval;
		String[] reportString;
		String[] slipString;
		
		readCSV();
		
		if (failed) return;
		
		for(int i=0;i<reportLines.size();i++){
			//Go through samples report lines first
			reportIDval = reportID.get(i);
			reportString = reportLines.get(i);
			//Search for corresponding line in the Coriell file
			if(slipID.contains(reportIDval)){
				index = slipID.indexOf(reportIDval);
				slipID.remove(index);
				slipString = slipLines.remove(index);
				//Run the comparison
				compare(reportString, slipString);
				try {
					//Put the information together in the concatenated CSV
					concatCSV.append(makeSiteString(reportString) + makeString(slipString) + "\n");
				} catch (IOException e) {
					e.printStackTrace();
				}
			} 
			else{
				//If the Sample is not present in the Coriell file, log it
				try {
					csv.append(reportIDval.concat(",not in Coriell\n"));
					concatCSV.append(makeString(reportString) + "\n");
				} catch (IOException e1) {
					e1.printStackTrace();
				}
			}
		}
		//Any samples in the Coriell file that can't be found in the samples report
		//should also be noted
		for(int i=0;i<slipLines.size();i++){
			try {
				csv.append("," + slipID.get(i) + ",not found in Sample Report\n");
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		try {
			csv.flush();
			csv.close();
			concatCSV.flush();
			concatCSV.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		setCompleted(true);
	}
	
	public void setCSV(FileWriter out){
		this.csv = out;
	}
	
	public void setConcatCSV(FileWriter out){
		this.concatCSV = out;
	}
	
	public void setReportFile(File report){
		this.reportFile = report;
	}
	
	public void setSlipFile(File slip){
		this.slipFile = slip;
	}
	
	/**
	 * Compare the information from the samples report to the organized lines
	 * from the Coriell file. Any time differences occur, log them in the CSV
	 * and show the values in both files.
	 * @param reportString
	 * @param slipString
	 */
	private void compare(String[] reportString, String[] slipString){

		String base = new String(slipString[0] + ",," );
		String export = new String();
		
		//Container check: Yes, I know it's bad
		reportString[9] = reportString[9].replace("DNA", "Whole Blood");
		reportString[9] = reportString[9].replace("RNA", "Whole Blood");
		if(! (slipString[1].equalsIgnoreCase(reportString[9])
				|| slipString[1].toLowerCase().contains(reportString[9].toLowerCase())
				|| reportString[9].toLowerCase().contains(slipString[1].toLowerCase())
				)) export+=reportString[9] + "," + slipString[1] + ",";
		else export += ",,";
		//Date Check
		if(!slipString[3].equals(reportString[8].trim())) {
			//System.out.println(reportString[7] + " " + slipString[3]);
			export+=reportString[8] + "," + slipString[3] + ",";
		}
		else export += ",,";
		//GUID Check
		if(!slipString[5].equalsIgnoreCase(reportString[2].trim())) export+=reportString[2] + "," + slipString[5] + ",";
		else export += ",,";
		//Gender Check
		if(!slipString[6].equalsIgnoreCase(reportString[4].trim())) export+=reportString[4] + "," + slipString[6] + ",";
		else export += ",,";
		//Site Check: Don't complain, I know it's ugly
		if(! (slipString[7].equalsIgnoreCase(reportString[0])
				|| slipString[7].toLowerCase().contains(reportString[0].toLowerCase())
				|| reportString[0].toLowerCase().contains(slipString[7].toLowerCase())
				)) export+=reportString[0] + "," + slipString[7] + ",";
		else export += ",,";
		if(!slipString[8].equals(reportString[5])){
			export+=reportString[5] + "," + slipString[8];
		}
		else export += ",,";

		if(! (export.split(",").length == 0)){
			try {
				export = base.concat(export);
				csv.append(export.concat("\n"));
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}
	
	/**
	 * Organizes the input lines from the Coriell file so only the important
	 * info is accounted for.
	 * @param input
	 * @return
	 * @throws NullPointerException
	 */
	private String[] organizeInput(String[] input) throws NullPointerException{
		String[] organized = new String[9];
		String[] tempArray;
		String tempStr;
		organized[0] = input[indecies[0]].trim(); //PID
		organized[1] = input[indecies[3]].trim() + "/" + input[indecies[4]].trim(); //Specimen
		
		tempStr = input[indecies[7]].trim();
		while(tempStr.endsWith("0") || tempStr.endsWith(".")){
			tempStr = tempStr.substring(0, tempStr.length()-1);
		}
		
		organized[2] = tempStr + " " + input[indecies[8]].trim(); //Volume
		if(input[indecies[9]] == "null")
			organized[3] = "null";
		else{
			//Need to change up the format of the dates because they are 
			//formatted really strangely
			organized[3] = input[indecies[9]].trim().split(" ")[0];
			if(organized[3].contains("-") || organized[3].contains("/")){
				if(organized[3].contains("-"))
					tempArray = organized[3].split("-");
				else tempArray = organized[3].split("/");
				tempStr = tempArray[0];
				if(tempArray[1].startsWith("0"))
					tempArray[0] = tempArray[1].replace("0", "");
				else tempArray[0] = tempArray[1];
				if(tempArray[2].startsWith("0"))
					tempArray[1] = tempArray[2].replace("0", "");
				else tempArray[1] = tempArray[2];
				tempArray[2] = tempStr;
			} else {
				tempArray = new String[]{"0", "0", "0"};
			}
			organized[3] = tempArray[2] + "/" + tempArray[0] + "/" + tempArray[1];
		}//Collection Date
		
		if(input[indecies[10]] == "null")
			organized[4] = "null";
		else{
			organized[4] = input[indecies[10]].trim().split(" ")[0];
			if(organized[4].contains("-") || organized[4].contains("/")){
				if(organized[4].contains("-"))
					tempArray = organized[4].split("-");
				else tempArray = organized[4].split("/");
				tempStr = tempArray[0];
				if(tempArray[1].startsWith("0"))
					tempArray[0] = tempArray[1].replace("0", "");
				else tempArray[0] = tempArray[1];
				if(tempArray[2].startsWith("0"))
					tempArray[1] = tempArray[2].replace("0", "");
				else tempArray[1] = tempArray[2];
				tempArray[2] = tempStr;
			} else {
				tempArray = new String[]{"0", "0", "0"};
			}
			organized[4] = tempArray[2] + "/" + tempArray[0] + "/" + tempArray[1];
		}//Date Received
		
		organized[5] = input[indecies[2]].trim(); //GUID
		organized[6] = input[indecies[1]].trim(); //Gender
		organized[7] = input[indecies[6]].trim(); //Site
		organized[8] = input[indecies[5]].trim(); //Age
		
		return organized;
	}
	
	/**
	 * Builds the string for the concatenated CSV.
	 * @param array
	 * @return
	 */
	private String makeString(String[] array){
		String cont = new String();
		for(int i=0; i<array.length; i++){
			if(array[i].equals("null")){
				cont += ",";
			}
			else
				cont += array[i] + ",";
		}
		return cont;
	}
	
	/**
	 * Used to build the string for output into the 
	 * concatention CSV. This version limits the
	 * length of the output array since it tends
	 * to be inconsistent (when it shouldn't be)
	 * @param array
	 * @return
	 */
	private String makeSiteString(String[] array){
		String cont = new String();
		int len = array.length;
		for(int i=0;i<11;i++){
			if(i>=len || array[i].equals("null"))
				cont += ",";
			else
				cont += array[i] + ",";	
		}
		
		return cont;
	}
	
	//Coriell
	
	/**
	 * Parse lines in the Coriell file. Things keep changing between
	 * semi-colon delimited and comma delimited, so just check for
	 * both to determine which one to use to split the line
	 * @param line
	 * @return
	 */
	private String[] parseLine(String line){
		String[] output = new String[20];
		int cnt = 0;
		int ind = 0;
		String delimiter;
		if(line.contains(";"))
			delimiter = ";";
		else delimiter = ",";
		while(cnt<20){
			ind = line.indexOf(delimiter);
			if(ind > 0)
				output[cnt] = line.substring(0, ind).trim();
			else if (ind == -1) break;
			else if (ind == 0) output[cnt] = "null";
			line = line.substring(ind+1);
			cnt++;
		}
		
		return output;
	}
	
	//Report
	
	private String[] parseLine2(String line){
		String[] output = new String[20];
		int cnt = 0;
		int ind = 0;
		while(cnt<20){
			ind = line.indexOf(",");
			if(ind > 0)
				output[cnt] = line.substring(0, ind).trim();
			else if (ind == -1) break;
			else output[cnt] = "null";
			cnt++;
			line = line.substring(ind+1);
		}
		String[] realout = new String[cnt];
		for(int i=0;i<cnt;i++){
			realout[i] = output[i];
		}
		
		return realout;
	}
	
	/**
	 * Read both CSV files and pull out the relevant information
	 * to compare
	 */

	private void readCSV(){
		
		int cnt = 0;

		String[] lineArray;
		try {
			BufferedReader input =  new BufferedReader(new FileReader(reportFile));
			try {
				String line = null; 
				reportLines = new Vector<String[]>();
				reportID = new Vector<String>();
				String PID;
				
				//Read the samples report file
				while (( line = input.readLine()) != null){
					cnt++;
					lineArray = parseLine2(line);
					if(lineArray.length != 0){
						PID = lineArray[10];
						if(PID == "null")
							continue;
						else if(reportID.contains(PID)){
							//Log the ID if it has already appeared once
							csv.append(PID + ",duplicated\n");
						}
						else{
							reportLines.add(lineArray);
							reportID.add(PID);
						}
					}
				}
			}catch(ArrayIndexOutOfBoundsException e){
				MipavUtil.displayError("Exception in Report File. Check line " 
						+ String.valueOf(cnt) + " of file for any errors.");
				failed = true;
			}catch(NullPointerException e){
				MipavUtil.displayError("Line " + String.valueOf(cnt) + " is "
						+ "incomplete in Report file. Please check line for missing information");
				failed = true;
			}
			finally {
				input.close();
			}
			input =  new BufferedReader(new FileReader(slipFile));
			try {
				cnt = 0;
				String line = null; 
				slipLines = new Vector<String[]>();
				slipID = new Vector<String>();
				
				//Read the header of the Coriell file to determine which
				//columns you need to read, and which ones go where
				String header = input.readLine();
				String[] headerArray = header.split(",");
				for(int i=0;i<headerArray.length;i++){
					String col = headerArray[i].trim();
					if(col.equals("*SampleId"))
						indecies[0] = i;
					else if(col.equals("*Gender"))
						indecies[1] = i;
					else if(col.equals("*Alias_ID"))
						indecies[2] = i;
					else if(col.equals("*Type"))
						indecies[3] = i;
					else if(col.equals("*Container_Type"))
						indecies[4] = i;
					else if(col.equals("*AgeYrs"))
						indecies[5] = i;
					else if(col.equals("*Site_ID"))
						indecies[6] = i;
					else if(col.equals("*Volume_Received"))
						indecies[7] = i;
					else if(col.equals("*Volume_Received_Units"))
						indecies[8] = i;
					else if(col.equals("*Collection_Date"))
						indecies[9] = i;
					else if(col.equals("*Received_Date"))
						indecies[10] = i;
				}
				for(int i=0;i<indecies.length;i++){
					if(indecies[i] == -1){
						MipavUtil.displayError("A header is missing");
						System.err.println(i);
						failed = true;
						return;
					}
				}
				
				//Delimiter also keeps changing, keep an eye on that
				while (( line = input.readLine()) != null){
					cnt++;
					System.out.println(line);
					line = line.replace("\"", "");
					lineArray = parseLine(line); //semi-colon delimiter 
					if(lineArray.length !=0){
						slipLines.add(organizeInput(lineArray));
						slipID.add(lineArray[0]);
					}
				}
			}
			catch(ArrayIndexOutOfBoundsException e){
				e.printStackTrace();
				MipavUtil.displayError("Exception in Coriell File. Check line " 
						+ String.valueOf(cnt) + " of file for any errors.");
				failed = true;
			}
			catch(NullPointerException n){
				n.printStackTrace();
				MipavUtil.displayError("Line " + String.valueOf(cnt) + " is "
						+ "incomplete in Coriell file. Please check line for missing information");
				failed = true;
			}
			finally {
				input.close();
			}
		}
		catch (IOException ex){
			ex.printStackTrace();
		}
	}

}
