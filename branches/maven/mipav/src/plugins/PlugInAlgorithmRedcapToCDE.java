import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Map.Entry;
import java.util.TreeMap;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.view.MipavUtil;


public class PlugInAlgorithmRedcapToCDE extends AlgorithmBase {

	private File file;
	
	private ArrayList<String[]> fileLines;
	
	private String submitName;
	
	private String stewardName;
	
	private String keyword;
	
	/**
	 * Correctly compare the permissible values
	 * so that numbers compare correctly instead
	 * or lexographically comparing numbers
	 */
	private Comparator<String> compare;
	
	public PlugInAlgorithmRedcapToCDE(File input, String subName, String stewName, String key){
		file = input; 
		fileLines = new ArrayList<String[]>();
		submitName = subName;
		stewardName = stewName;
		keyword = key;
		
		compare = new Comparator<String>(){

			@Override
			public int compare(String o1, String o2) {
				try{
					float n1 = Float.valueOf(o1);
					float n2 = Float.valueOf(o2);
					if(n1 > n2)
						return 1;
					else if(n1 < n2)
						return -1;
					else return 0;
				} catch (NumberFormatException e){
					//If one or both are not numbers, 
					//compare lexographically
					return o1.compareTo(o2);
				}
			}
			
		};
	}
	
	@Override
	public void runAlgorithm() {
		readCSV();
		//initialize output CSV
		File outCSV = initOutputCSV();
		if(outCSV == null)
			return;
		try{
		
			FileWriter wr = new FileWriter(outCSV, true);
			for(int i=0;i<fileLines.size();i++){
				
				String[] line = fileLines.get(i);
				String name = convertName(line[0]);
				String title = line[4];
				String desc = title;
				String type = "";
				String size = "";
				String field = line[3].trim();
				String values = "";
				String valueDesc = "";
				String min = line[8];
				String max = line[9];
				String restriction = "";
				String units = ""; //No units in Redcap
				String notes = line[11];
				String keyValues = "";
				
				if(field.equals("text")){
					if(line[7].startsWith("date"))
						type = "Date or Date & Time";
					else if(line[7].startsWith("number") || 
							line[7].equals("integer"))
						type = "Numeric Values";
					else type = "Alphanumeric";
					size = "255";
					restriction = "Free-Form Entry";
				}else if(field.equals("notes")){
					type = "Alphanumeric";
					size = "4000";
					restriction = "Free-Form Entry";
				}else if(field.equals("dropdown") || 
						field.equals("checkbox") ||
						field.equals("radio")){
					TreeMap<String, String> pairs = parseChoices(line[5]);
					Entry<String, String> pair;
					type = "Numeric Values";
					while((pair = pairs.pollFirstEntry()) != null){
						values += pair.getKey() + ";";
						valueDesc += pair.getValue() + ";";
						try{
							Integer.valueOf(pair.getKey());
						} catch (NumberFormatException e){
							type = "Alphanumeric";
						}
					}
					values = values.substring(0, values.length() - 1);
					valueDesc = valueDesc.substring(0, valueDesc.length() - 1);
					
					if(values.contains(","))
						values = "\"" + values + "\"";
					if(valueDesc.contains(","))
						valueDesc = "\"" + valueDesc + "\"";
					if(field.equals("checkbox"))
						restriction = "Multiple Pre-Defined Values Selected";
					else restriction = "Single Pre-Defined Value Selected";
					
					if(type.equals("Numeric Values")){
						type = "Alphanumeric";
						keyValues += "Code values [" + values + "] ";
						values = valueDesc;
					}
				}else if(field.equals("calc")){
					//what to do here? no clue right now
					type = "Numeric Values";
					restriction = "Free-Form Entry";
					if(notes.equals(""))
						notes = line[5];
					else notes += " | " + line[5];
				}else if(field.equals("yesno")){
					values = "Yes;No";
					valueDesc = "Yes;No";
					type = "Alphanumeric";
					restriction = "Single Pre-Defined Value Selected";
					keyValues += "Code values [1;0]";
				}else{
					
				}
				
				String output = name + "," + title + ",Unique Data Element,," + desc + ","
						+ type + "," + size + "," + restriction + "," + min + "," + max + ","
						+ values + "," + valueDesc + "," + units + "," + notes + "," + keyValues
						+ ",," + keyword + ",,,,,,,,,,,Adult;Pediatric"
						+ ",,,,,,,,,,,,,,,,,,Supplemental,Supplemental,Supplemental,Supplemental,"
						+ ",,,,,,,,,,,,,," + submitName + ",,,"
						+ stewardName + "\r\n";
				
				wr.write(output);
			}
			
			wr.close();
		} catch(IOException e){
			MipavUtil.displayError("Could not write to output file");
			e.printStackTrace();
			return;
		}
		
		setCompleted(true);
	}
	
	private void readCSV(){
		
		try {
			final BufferedReader input = new BufferedReader(new FileReader(file));
			String line = null;
			input.readLine();//remove header
			
			while((line = input.readLine()) != null){
				fileLines.add(parseLine(line));
			}
			
			input.close();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}

	}
	
	private String[] parseLine(String line){
		
		ArrayList<String> parts = new ArrayList<String>();
		
		while(line.indexOf(",") > -1){
			if(line.startsWith(",")){
				parts.add("");
				line = line.substring(1);
				continue;
			}
			int next = line.indexOf(",");
			if(line.startsWith("\"")){
				int check = line.indexOf("\"\"");
				int end = line.indexOf("\"", 1);
				while(check == end){
					check = line.indexOf("\"\"", end+2);
					end = line.indexOf("\"", end+2);
				}
				next = line.indexOf(",", end);
			}
			String substring = line.substring(0, next);
			parts.add(substring);
			line = line.substring(next+1);
		}
		
		parts.add(line);
		
		String[] out = new String[parts.size()];
		for(int i=0;i<parts.size();i++){
			out[i] = parts.get(i);
		}
		
		return out;
	}

	private TreeMap<String, String> parseChoices(String choices){
		//Assume you know it's a choice field
		if(choices.startsWith("\""))
			choices = choices.substring(1,choices.length()-1);
		String[] split = choices.split("\\|");
		TreeMap<String, String> pairs = new TreeMap<String, String>(compare);
		for(int i=0;i<split.length;i++){
			String[] pair = split[i].split(",");
			pairs.put(pair[0].trim(), pair[1].trim());
		}
		return pairs;
	}
	
	private String convertName(String name){
		String output = Character.toUpperCase(name.charAt(0)) + name.substring(1);
				//name.replace(name.charAt(0), Character.toUpperCase(name.charAt(0)));
		while(output.indexOf("_") != -1){
			int ind = output.indexOf("_");
			output = output.replaceFirst("_", "");
			char c = Character.toUpperCase(output.charAt(ind));
			output = output.substring(0, ind) + c + output.substring(ind+1);
		}
		return output;
	}
	
	private File initOutputCSV(){
		
		String parent = file.getParent();
		String origName = file.getName();
		String outName = origName.substring(0, origName.lastIndexOf("."));
		outName += "_CDE.csv";
		
		File outputCSV = new File(parent + File.separator + outName);
		
		try {
			FileWriter fw = new FileWriter(outputCSV);
			String header = "Variable Name,Title,Element Type,Definition,"
					+ "Short Description,Datatype,Maximum Character Quantity,"
					+ "Input Restriction,Minimum Value,Maximum Value,"
					+ "Permissible Values,Permissible Value Descriptions,"
					+ "Unit of Measure,Guidelines/Instructions,Notes,"
					+ "Preferred Question Text,Keywords,References,"
					+ "Historical Notes,See Also,Effective Date,Until Date,"
					+ "External ID.LOINC,External ID.SNOMED,External ID.caDSR,"
					+ "External ID.CDISC,External ID.NINDS,Population.All,"
					+ "Domain.General (For all diseases),Domain.Traumatic Brain Injury,"
					+ "Domain.Parkinson's disease,Domain.Friedreich's Ataxia,Domain.Stroke,"
					+ "Domain.Amyotrophic Lateral Sclerosis,Domain.Huntington's Disease,"
					+ "Domain.Multiple Sclerosis,Domain.Neuromuscular Diseases,"
					+ "Domain.Myasthenia Gravis,Domain.Spinal Muscular Atrophy,"
					+ "Domain.Duchenne Muscular Dystrophy/Becker Muscular Dystrophy,"
					+ "Domain.Congenital Muscular Dystrophy,Domain.Spinal Cord Injury,"
					+ "Domain.Headache,Domain.Epilepsy,Classification.General (For all diseases),"
					+ "Classification.Acute Hospitalized,Classification.Concussion/Mild TBI,"
					+ "Classification.Epidemiology,Classification.Moderate/Severe TBI: "
					+ "Rehabilitation,Classification.Parkinson's Disease,Classification."
					+ "Friedreich's Ataxia,Classification.Stroke,Classification.Amyotrophic "
					+ "Lateral Sclerosis,Classification.Huntington's Disease,Classification"
					+ ".Multiple Sclerosis,Classification.Neuromuscular Diseases,"
					+ "Classification.Myasthenia Gravis,Classification.Spinal Muscular "
					+ "atrophy,Classification.Duchenne Muscular Dystrophy/Becker Muscular "
					+ "Dystrophy,Classification.Congenital Muscular Dystrophy,Classification."
					+ "Spinal cord Injury,Classification.Headache,Classification.Epilepsy,"
					+ "Submitting Organization Name,Submitting Contact Name,Submitting "
					+ "Contact Information,Steward Organization Name,Steward Contact Name,"
					+ "Steward Contact Information\n";
			fw.write(header);
			fw.close();
		} catch (IOException e) {
			MipavUtil.displayError("Could not initialize output CSV");
			e.printStackTrace();
			return null;
		}
		
		return outputCSV;
	}
}
