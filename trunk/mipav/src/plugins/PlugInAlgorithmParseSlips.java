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
	
	private Vector<String[]> reportLines;
	private Vector<String[]> slipLines;
	private Vector<String> reportID;
	private Vector<String> slipID;
	private File reportFile;
	private File slipFile;
	private FileWriter csv;
	private FileWriter concatCSV;
	private boolean removeHeader;
	private boolean failed;
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
		// TODO Auto-generated method stub
		int index;
		String reportIDval;
		String[] reportString;
		String[] slipString;
		readCSV();
		if (failed) return;
		for(int i=0;i<reportLines.size();i++){
			reportIDval = reportID.get(i);
			reportString = reportLines.get(i);
			if(slipID.contains(reportIDval)){
				index = slipID.indexOf(reportIDval);
				slipID.remove(index);
				slipString = slipLines.remove(index);
				compare(reportString, slipString);
				try {
					concatCSV.append(makeSiteString(reportString) + makeString(slipString) + "\n");
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			} 
			else{
				try {
					csv.append(reportIDval.concat(",not in Coriell\n"));
					concatCSV.append(makeString(reportString) + "\n");
				} catch (IOException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
			}
		}
		for(int i=0;i<slipLines.size();i++){
			try {
				csv.append("," + slipID.get(i) + ",not found in Sample Report\n");
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		try {
			csv.flush();
			csv.close();
			concatCSV.flush();
			concatCSV.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
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
	
	public void removeHeader(boolean remove){
		removeHeader = remove;
	}
	
	public void setReportFile(File report){
		this.reportFile = report;
	}
	
	public void setSlipFile(File slip){
		this.slipFile = slip;
	}
	
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
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}
	
	private String[] organizeInput(String[] input) throws NullPointerException{
		String[] organized = new String[9];
		String[] tempArray;
		String tempStr;
		//System.out.println(makeString(input));
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
			organized[3] = tempArray[0] + "/" + tempArray[1] + "/" + tempArray[2];
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
			organized[4] = tempArray[0] + "/" + tempArray[1] + "/" + tempArray[2];
		}//Date Received
		
		organized[5] = input[indecies[2]].trim(); //GUID
		organized[6] = input[indecies[1]].trim(); //Gender
		organized[7] = input[indecies[6]].trim(); //Site
		organized[8] = input[indecies[5]].trim();
		
		return organized;
	}
	
	private String makeString(String[] array){
		String cont = new String();
		for(int i=0; i<array.length; i++){
			if(array[i].equals("null")){
				cont += ",";
			}
			else
				cont += array[i] + ",";
		}
		//cont += "\n";
		return cont;
	}
	
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
				
				while (( line = input.readLine()) != null){
					cnt++;
					//System.out.println(line);
					lineArray = parseLine2(line);
					if(lineArray.length != 0){
						PID = lineArray[10];
						if(PID == "null")
							continue;
						else if(reportID.contains(PID)){
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
				/*if (removeHeader) {
					input.readLine(); //Strip header from Coriell file
					cnt++;
				}*/
				String header = input.readLine();
				System.err.println(header);
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
