import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.view.MipavUtil;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Vector;

/**
 * Quick algorithm plugin used to check how many "complete" samples
 * are present in the provided file. A complete sample only occurs when
 * all samples from the Baseline visit to the most recent visit are in
 * the file. For example, if a particular sample ID has samples for 
 * Baseline, 6 months, 12 months, and 18 months, that constitutes a
 * complete sample, but if the 6 or 12 month sample was missing, then
 * it would no longer be a complete set. 
 * @author wangvg
 *
 */
public class PlugInAlgorithmCompleteCatalog extends AlgorithmBase {

	/**
	 * File that you are counting
	 */
    private final File catalogFile;

    /**
     * Output CSV to tabulate how many complete samples
     * exist per sample type
     */
    private final FileWriter csv;

    /**
     * List of IDs that are in the file
     */
    private Vector<String> catalogID;

    /**
     * List of visits associated with the ID
     */
    private Vector<String> visit;

    /**
     * List of sample types associated with the ID
     */
    private Vector<Integer> type;

    /**
     * Array to keep track of the number of complete samples
     */
    private int[] complete;

    /**
     * Indecies to determine which columns contain the 
     * information you are looking for
     */
    private int GUID = -1;
    
    private int specimen = -1;
    
    private int visitID = -1;

    public PlugInAlgorithmCompleteCatalog(final File file, final FileWriter out/*,final int lines*/) {
        super();
        catalogFile = file;
        csv = out;
    }

    @Override
    public void runAlgorithm() {
        complete = new int[6]; // RNA, Plasma, Serum, CSF, Blood, DNA
        final String[] types = {"RNA", "Plasma", "Serum", "CSF", "Blood", "DNA"};
        String GUIDStr;
        Integer typeNum;

        readCSV();
        
        while (catalogID.size() > 0) {
            GUIDStr = catalogID.get(0);
            typeNum = type.get(0);
            //Perform the comparison given the ID and sample type
            //at the top of the stack
            if (compare(GUIDStr, typeNum)) {
                complete[typeNum.intValue()]++;
            }
        }

        try {
        	//Write out results from the counting to the CSV
            csv.append("Sample Type,Number of Complete Sets\n");
            for (int i = 0; i < 6; i++) {
                csv.append(types[i] + "," + String.valueOf(complete[i]) + "\n");
            }
            csv.flush();
            csv.close();

        } catch (final IOException e) {
            e.printStackTrace();
        }
        
        setCompleted(true);

    }

    /**
     * Comparison method to determine if the given GUID and Sample type
     * constitutes a complete set. 
     * @param GUID
     * @param which
     * @return
     */
    private boolean compare(final String GUID, final Integer which) {

        // First element is not removed already from each vector

        int index;
        int max;
        int month;
        int num;
        String[] visitType;
        final Vector<String> IDvisit = new Vector<String>();
        IDvisit.add(visit.get(0));
        while (catalogID.contains(GUID)) {
        	//Search for the other GUIDS that match this GUID
        	//and sample type
            index = catalogID.indexOf(GUID);
            while ( !type.get(index).equals(which)) {
                index = catalogID.indexOf(GUID, index + 1);
                if (index == -1) {
                    break;
                }
            }
            if (index == -1) {
                break;
            }
            catalogID.remove(index);
            if ( !IDvisit.contains(visit.get(index))) {
                IDvisit.add(visit.get(index));
            }
            visit.remove(index);
            type.remove(index);

        }

        max = 0;
        
        //If only Baseline is present, or even if only
        //one sample visit is present, it automatically
        //cannot be a complete set
        if(IDvisit.size()<2) return false;
        
        //Determine the timeline of the last visit, so
        //find the largest number
        for (int i = 0; i < IDvisit.size(); i++) {
            visitType = IDvisit.get(i).split(" ");
            if (visitType.length == 2) {
                month = Integer.parseInt(visitType[0]);
                if (month > max) {
                    max = month;
                }
            }
        }
        
        //Visits are evenly scheduled, so given a certain
        //number of months, there should be that many visits,
        //or else there aren't enough samples.
        if(which.equals(new Integer(3)))
        	num = max / 12 + 1;
        else
        	num = max / 6 + 1;
        return (num == IDvisit.size());
    }

    /**
     * Read the catalog file and log the relevant information
     * for comparison and counting.
     */
    private void readCSV() {

        String[] lineArray;
        String typeStr;
        int typeNum;
        try {
            final BufferedReader input = new BufferedReader(new FileReader(catalogFile));
            try {
                String line = null;
                catalogID = new Vector<String>();
                visit = new Vector<String>();
                type = new Vector<Integer>();
                
                //Read the header to determine which columns
                //contain the info we want
                String header = input.readLine();
                String[] hArray = parseLine2(header);
                for(int i=0;i<hArray.length;i++){
                	String hStr = hArray[i].trim();
                	
                	if(hStr.contains("Catalog ID"))
                		GUID = i;
                	else if(hStr.contains("Clinical Event"))
                		visitID = i;
                	else if(hStr.contains("Type"))
                		specimen = i;
                }
                
                if(GUID == -1 || visitID == -1 || specimen == -1){
                	MipavUtil.displayError("A header is missing");
                	return;
                }

                //Read the information and store the sample
                //type based on the numbers specified at the
                //beginning
                while ( (line = input.readLine()) != null) {
                    lineArray = parseLine2(line);
                    if (lineArray.length != 0) {

                        typeStr = lineArray[specimen].trim().toLowerCase();

                        if (typeStr.contains("rna")) {
                            typeNum = 0;
                        } else if (typeStr.contains("plasma")) {
                            typeNum = 1;
                        } else if (typeStr.contains("serum")) {
                            typeNum = 2;
                        } else if (typeStr.contains("csf")) {
                            typeNum = 3;
                        } else if (typeStr.contains("blood")) {
                            typeNum = 4;
                        } else if (typeStr.contains("dna")){
                        	typeNum = 5;
                        } else {
                            typeNum = -1;
                        }

                        if (typeNum != -1) {
                            type.add(new Integer(typeNum));
                            catalogID.add(lineArray[GUID].trim());
                            visit.add(lineArray[visitID].trim());
                        }
                    }
                }
            } finally {
                input.close();
            }
        } catch (final IOException ex) {
            ex.printStackTrace();
        }
    }
    
    /**
     * Parses the lines from the catalog file. The same as
     * can be found in the ParseSlips algorithms. 
     * @param line
     * @return
     */
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
	
}
