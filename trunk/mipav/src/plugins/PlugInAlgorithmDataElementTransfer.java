import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.view.MipavUtil;


public class PlugInAlgorithmDataElementTransfer extends AlgorithmBase {
	
	private File inFile;
	
	private ArrayList<String[]> fileLines;

	@Override
	public void runAlgorithm() {
		// TODO Auto-generated method stub
		
		readCSV();
		String[] line;
		String output = "";
		for(int i=0;i<fileLines.size();i++){
			line = fileLines.get(i);
			output += adjustName(line[0]) + ",,,";
			output += determineType(line[7].trim(), line[6].trim() ) + ",";
			output += line[1] + ",";
			output += parseEntry(line[10], line[11]) + ",";
			output += line[12] + ",";
			if(line[5].equals("Date or Date & Time"))
				output += "date_ymd,";
			else if(line[5].equalsIgnoreCase("Numeric Values"))
				output += "numeric,"
			else output += ",";
			output += line[8] + "," + line[9] + ",";
			
			
		}
		

	}
	
	private String adjustName(String input){
		String output;
		int length = input.length();
		output = input.toLowerCase();
		if(length > 26)
			output = output.substring(0, 26);
		return output;
		
	}
	
	private String determineType(String input, String size){
		
		String output;
		if(input.equalsIgnoreCase("Free-Form Entry")){
			if(size.equals("") || size.equals("255"))
				output = "text";
			else output = "notes";
		}
		else if(input.equalsIgnoreCase("Single Pre-Defined Value Selected"))
			output = "dropdown";
		else if(input.equalsIgnoreCase("Multiple Pre-Defined Values Selected"))
			output = "checkbox";
		else output = "N/A";
		
		return output;
	}
	
	private String parseEntry(String valueIn, String descIn){
		String output = "";
		if(valueIn.isEmpty() || descIn.isEmpty()){
			return "";
		}
		
		output += "\"";
		//String[] values = new String[20];
		//String[] desc = new String[20];
		String[] values = valueIn.split(";");
		String[] desc = descIn.split(";");
		
		
		output += "\"";
		return output;
	}
	
	private String[] parseLine(String line){
		String[] output = new String[20];
		int cnt = 0;
		int ind;
		int check;
		
		while(cnt<12){
			ind = -1;
			check = -1;
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
			else{
				ind = line.indexOf(",");
				if(ind > 0){
					output[cnt] = line.substring(0, ind).trim();
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
	
	private void readCSV(){

		int cnt = 0;

		String[] lineArray;
		BufferedReader input;
		try {
			input =  new BufferedReader(new FileReader(inFile));
			String line = null; 
			fileLines = new ArrayList<String[]>();

			while (( line = input.readLine()) != null){
				cnt++;
				//System.out.println(line);
				lineArray = parseLine(line);
				if(lineArray.length != 0){
					fileLines.add(lineArray);
				}
			}
			
			input.close();
		}
		catch (IOException ex){
			ex.printStackTrace();
		}catch(ArrayIndexOutOfBoundsException e){
			MipavUtil.displayError("Exception in Report File. Check line " 
					+ String.valueOf(cnt) + " of file for any errors.");
			
		}
	}


}
