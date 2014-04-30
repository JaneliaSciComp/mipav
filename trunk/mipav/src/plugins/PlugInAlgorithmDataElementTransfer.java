import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Scanner;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.view.MipavUtil;


public class PlugInAlgorithmDataElementTransfer extends AlgorithmBase {
	
	/**
	 * Writer to the output file (converted REDCap dictionary)
	 */
	private FileWriter csv;
	
	/**
	 * File containing the CDE to covert to REDCap format
	 */
	private File inFile;
	
	/**
	 * Rows pulled from the input CSV
	 */
	private ArrayList<String[]> fileLines;
	
	public PlugInAlgorithmDataElementTransfer(File input){
		
		//Output file is saved to the same directory
		//With same name except with _redcap appended
		inFile = input;
		String name = inFile.getName();
		name = name.substring(0, name.indexOf("."));
		String parent = inFile.getParent() + File.separator;
		File outFile = new File(parent + name + "_redcap.csv");
		try {
			csv = new FileWriter(outFile, false);
		} catch (IOException e) {
			MipavUtil.displayError("Unable to open CSV file for write");
			return;
		}
	}

	@Override
	public void runAlgorithm() {
		int i=0;
		try{
			if(readCSV() ||	initCSV()) 
				return;
	
			String[] line;
			String output;
			
			for(;i<fileLines.size();i++){
				output = "";
				line = fileLines.get(i);
				/*for(int j=0;j<line.length;j++){
					System.out.print(line[j] + ",");
				}
				System.out.print("\n");*/
				//New branching statement here to account for Other, specify
				//and DateTime variables
				/*if(line[0].contains("DateTime") &&
						line[5].equalsIgnoreCase("Date or Date & Time")){
					String outputSplit = "";
					outputSplit += line[0] + ",";
					output += line[0] + "TimeSplit,";
					outputSplit += adjustName(line[0]) + ",,,";
					output += ",,,";
					outputSplit += determineType(line) + ",";
					output += determineType(line) + ",";
					outputSplit += line[1] + ",";
					output += ",";
					outputSplit += ",";
					output += ",";
					outputSplit += "YYYY-MM-DD,";
					output += "HH:MM (24-hour)";
					outputSplit += "date_ymd,";
					output += "time,";
					csv.append(outputSplit + "\r\n");
					
				} else*/ if(line[fieldType].equalsIgnoreCase("Free-Form Entry")
						&& line[permValDesc].contains("Other, specify")){
					String outputSplit = "";
					outputSplit += line[varName] + ",";
					output += line[varName] + "OtherSplit,";
					outputSplit += adjustName(line[varName]) + ",,,";
					output += ",,,";
					outputSplit += "dropdown,";
					output += "text,";
					outputSplit += line[title] + ",";
					output += ",";
					
					String entries = parseEntry(line[permValues], line[permValDesc]);
					outputSplit += entries + ",";
					
					output += ",";
					outputSplit += ",";
					output += ",";
					outputSplit += line[measureType] + ",";
					output += line[measureType] +",";
					
					output += ",,,,";
					
					int ind = entries.lastIndexOf("|");
					String last = entries.substring(ind + 2).trim();
					String value = last.substring(0, last.indexOf(","));
					output += "[" + adjustName(line[varName]) + "] = \"" + value + "\",";
					
					csv.append(outputSplit + "\r\n");
				}
				else{
					//Append original variable name for easier reversal
					output += line[varName] + ",";
					//Appends Variable Name. Form Name and Section Header
					//are not present in CDE, so skip for now
					output += adjustName(line[varName]) + ",,,";
					//Appends the field type
					output += determineType(line) + ",";
					//Appends the field label
					output += line[title] + ",";
					//Appends Choices/Calculations
					if(!line[fieldType].equalsIgnoreCase("Free-Form Entry"))
						output += parseEntry(line[permValues], line[permValDesc]);
					else output += ",,";
					//Appends Field Note
					output += line[measureType] + ",";
					//Appends Text Validation. Also, if it is a date, change 
					//field note to say YYYY-MM-DD
					if(line[dataType].equalsIgnoreCase("Date or Date & Time")){
						if(line[varName].contains("DateTime")){
							output = output.substring(0, output.length()-1) + "YYYY-MM-DD + HH:MD Military Time,";
							output += "datetime_ymd,";
						}
						else{
							output = output.substring(0, output.length()-1) + "YYYY-MM-DD,";
							output += "date_ymd,";
						}
					}
					else if(line[dataType].equalsIgnoreCase("Numeric Values") && line[fieldType].equalsIgnoreCase("Free-Form Entry"))
						output += "numeric,";
					else output += ",";
					//Appends Text Validation min/max
					output += line[minVal] + "," + line[maxVal] + ",";
				}
				//Output to the CSV
				csv.append(output + "\r\n");
	
			}
		
		}
		catch(IOException e){
			MipavUtil.displayError("Unable to write to CSV");
			return;
		}
		catch(Exception e){
			MipavUtil.displayError("Error during conversion at line: " + String.valueOf(i+2));
			e.printStackTrace();
			return;
		}
		try {
			csv.close();
		} catch (IOException e) {
			MipavUtil.displayError("Unable to close CSV file");
			return;
		}
		
		setCompleted(true);
		

	}
	
	/**
	 * Variable names must be truncated to 26 characters
	 * in the REDCap format. Also, the variable names must
	 * be in all lowercase. 
	 * 
	 * @param input
	 * @return
	 */
	private String adjustName(String input){
		
		String output;
		int length = input.length();
		output = input.toLowerCase();
		if(length > 26)
			output = output.substring(0, 26);
		return output;
		
	}
	
	/**
	 * Based on what format the variable is in the CDE, 
	 * choose a REDCap format. The decisions are described
	 * below.
	 * @param line
	 * @return
	 */
	private String determineType(String[] line){
		
		String input = line[fieldType];
		String size = line[fieldSize];
		String[] desc = line[permValDesc].split(";");
		
		String output;
		//There are two types of text entries in REDCap,
		//which are determined based on the given size
		if(input.equalsIgnoreCase("Free-Form Entry")){
			if(size.equals("") || size.equals("255"))
				output = "text";
			else output = "notes";
		}
		//If some of the options are missing, choose Radio
		//This will have to be custom tailored after output
		//As the user may want something to be a radio button
		//instead of a dropdown, or vice versa
		else if(input.equalsIgnoreCase("Single Pre-Defined Value Selected")){
			boolean sparse = false;
			for(int i=0;i<desc.length;i++){
				if(desc[i].isEmpty()) {
					sparse = true;
					break;
				}
			}
			if(sparse)
				output = "radio";
			else output = "dropdown";
		}
		else if(input.equalsIgnoreCase("Multiple Pre-Defined Values Selected"))
			output = "checkbox";
		else output = "N/A";
		
		return output;
	}
	
	
	/**
	 * Parse through the Permissible Values cell in order to
	 * covert into REDCap friendly form. Also makes a few
	 * consistency modifications for Yes/No selections
	 * @param valueIn
	 * @param descIn
	 * @return
	 */
	private String parseEntry(String valueIn, String descIn){
		String output = "";
		if(valueIn.isEmpty() || descIn.isEmpty()){
			return "";
		}
		
		valueIn = valueIn.replaceAll("\"", "");
		descIn = descIn.replaceAll("\"", "");
		
		output += "\"";

		String[] values = splitEntry(valueIn);
		String[] desc = splitEntry(descIn);
		String valueCell;
		String descCell;
		boolean allInts = true;
		//Check to see if all the values are integers or not
		for(int i=0;i<values.length;i++){
			try{
			Integer.parseInt(values[i]);
			}
			catch(NumberFormatException e){

				allInts = false;
				break;
			}
		}
		//Place the value with its associated label
		if(allInts){
			for(int i=0;i<values.length;i++){
				valueCell = values[i];
				descCell = desc[i];
				output += valueCell + ", " + descCell + " | ";
			}
		}
		//If Yes/No is not already attached to numeric values, make sure
		//Yes = 1, No = 0
		//Also check for Left/Right and Normal/Abnormal are consistent
		else if(values.length > 1 && ((values[0].startsWith("Yes") && values[1].startsWith("No")) 
					|| (values[0].equalsIgnoreCase("y") && values[1].equalsIgnoreCase("n"))
					|| (values[0].startsWith("Right") && values[1].startsWith("Left"))
					|| (values[0].equalsIgnoreCase("r") && values[1].equalsIgnoreCase("l"))
					|| (values[0].startsWith("Normal") && values[1].startsWith("Abnormal")))){
			String temp = values[1];
			values[1] = values[0];
			values[0] = temp;
			for(int i=0;i<values.length;i++){
				valueCell = values[i];
				output += String.valueOf(i) + ", " + values[i] + " | ";
			}
		}
		//Otherwise, just label in increasing order, and show 
		//both value and label (minus overlap)
		else{
			for(int i=0;i<values.length;i++){
				valueCell = values[i];
				descCell = desc[i];
				if(descCell.startsWith(valueCell))
					output += String.valueOf(i) + ", " + descCell + " | ";
				else if(descCell.equals(""))
					output += String.valueOf(i) + ", " + valueCell + " | ";
				else output += String.valueOf(i) + ", " + valueCell + " (" + descCell + ") | ";
			}
		}
		output = output.substring(0, output.length() - 2);
		output += "\",\"";
		
		//Now also add the numerical labels with permissible
		//values ONLY
		
		
		if(allInts){
			for(int i=0;i<values.length;i++){
				valueCell = values[i];
				output += valueCell + ", " + valueCell + " | ";
			}
		}
		else{
			for(int i=0;i<values.length;i++){
				valueCell = values[i];
				output += String.valueOf(i) + ", " + valueCell + " | ";
			}
		}
		output = output.substring(0, output.length() - 2);
		output += "\",";
		
		
		
		return output;
	}
	
	/**
	 * Split rows from the CSV into arrays. Part of the problem is with 
	 * quotation marks, so not only do you need to parse through commas, 
	 * but single and double quotations.
	 * @param line
	 * @return
	 */
	private String[] parseLine(String line){
		String[] output = new String[20];
		int cnt = 0;
		int ind;
		int check;
		
		while(cnt<20){
			ind = -1;
			check = -1;
			//Parse through sections with quotations marks
			if(line.startsWith("\"")){
				while(ind==check){
					ind = line.indexOf("\"", check + 2);
					check = line.indexOf("\"\"", check+2);
					if(check == -1) break;
				}
				output[cnt] = line.substring(0, ind+1);
				cnt++;
				line = line.substring(ind+2);
			}
			//Split by the commas
			else{
				ind = line.indexOf(",");
				if(ind > 0){
					output[cnt] = line.substring(0, ind).trim();
					cnt++;
				}
				else if(ind == 0){
					output[cnt] = "";
					cnt++;
				}
				else if (ind == -1) break;
				line = line.substring(ind+1);
			}
		}

		String[] realout = new String[cnt];
		for(int i=0;i<cnt;i++){
			realout[i] = output[i];
		}
		
		return realout;
	}
	
	private int varName = -1;
	private int fieldType = -1;
	private int title = -1;
	private int permValues = -1;
	private int permValDesc = -1;
	private int measureType = -1;
	private int dataType = -1;
	private int minVal = -1;
	private int maxVal = -1;
	private int fieldSize = -1;
	
	/**
	 * Initialize the output CSV header
	 * @return
	 */
	private boolean initCSV(){
		String header = "Original Name (Delete before importing into REDCAP),"
				+ "Variable / Field Name,Form Name,Section Header,Field Type,Field Label," 
				+ "Choices OR Calculations,"
				+ "Permissible Values (From NINDS CDE - Delete before importing into REDCAP),"
				+ "Field Note,Text Validation,Text Validation Min,"
				+ "Text Validation Max,Identifier?,Branching Logic,Required Field?,"
				+ "Custom Alignment,Question Number \n";
		boolean fail = false;
		try {
			csv.append(header);
		} catch (IOException e) {
			MipavUtil.displayError("Unable to write to CSV file");
			fail = true;
		}
		return fail;
	}
	
	/**
	 * Read the CSV. Due to CSV including line breaks in some cells, the
	 * delimiter to check for is \r\n instead of just \n. This could cause
	 * some problems, as this is a Windows-dependent solution.
	 * @return
	 */
	private boolean readCSV(){

		int cnt = 0;
		boolean fail = false;

		String[] lineArray;

		try {
			
			Scanner scan = new Scanner(inFile);
			Scanner lineScan = scan.useDelimiter("\\r\\n");
			fileLines = new ArrayList<String[]>();
			String line;
			String header = lineScan.next();
			String[] hArray = header.split(",");
			for(int i=0;i<hArray.length;i++){
				String hVal = hArray[i].trim();
				if(hVal.equals("Name"))
					varName = i;
				else if(hVal.equals("Title"))
					title = i;
				else if(hVal.equals("Size"))
					fieldSize = i;
				else if(hVal.equals("Input Restrictions"))
					fieldType = i;
				else if(hVal.equals("Permissible Values"))
					permValues = i;
				else if(hVal.equals("Permissible Value Descriptions"))
					permValDesc = i;
				else if(hVal.equals("Measurement Type"))
					measureType = i;
				else if(hVal.equals("Data Type"))
					dataType = i;
				else if(hVal.equals("Minimum Value"))
					minVal = i;
				else if(hVal.equals("Maximum Value"))
					maxVal = i;
			}
			if(varName == -1 || fieldType == -1 || title == -1 || permValues == -1
					|| permValDesc == -1 || measureType == -1 || dataType == -1 
					|| minVal == -1 || maxVal == -1 || fieldSize == -1){
				MipavUtil.displayError("A header is missing");
				return true;
			}
			while(lineScan.hasNext()){
				cnt++;
				line = scan.next();
				if(line.contains(",")){
					lineArray = parseLine(line);
					if(lineArray.length != 0){
						fileLines.add(lineArray);
					}
				}
			}
			scan.close();
		}
		catch (IOException ex){
			MipavUtil.displayError("Exception in Report File. Check line " 
					+ String.valueOf(cnt) + " of file for any errors.");
			fail = true;
		}catch(ArrayIndexOutOfBoundsException e){
			MipavUtil.displayError("Exception in Report File. Check line " 
					+ String.valueOf(cnt) + " of file for any errors.");
			fail = true;
		}
		return fail;
	}
	
	/**
	 * Actually goes through the entries in the Permissible
	 * Values cell and splits the values into a string array
	 * @param entry
	 * @return
	 */
	private String[] splitEntry(String entry){
		String[] out;
		ArrayList<String> values = new ArrayList<String>();
		//System.out.println(entry);
		String part;
		
		int index = entry.indexOf(";");
		
		while(index != -1){
			part = entry.substring(0, index);
			values.add(part);
			entry = entry.substring(index+1);
			index = entry.indexOf(";");
		}
		values.add(entry);
		out = values.toArray(new String[values.size()]);
		
		
		return out;
		
		
	}


}
