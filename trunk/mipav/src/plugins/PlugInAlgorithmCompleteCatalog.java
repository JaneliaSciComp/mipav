import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.view.MipavUtil;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Vector;


public class PlugInAlgorithmCompleteCatalog extends AlgorithmBase {

    private final File catalogFile;

    private final FileWriter csv;

    private Vector<String> catalogID;

    private Vector<String> visit;

    private Vector<Integer> type;

    private int[] complete;

    //private final int remove;
    
    private int GUID = -1;
    
    private int specimen = -1;
    
    private int visitID = -1;

    public PlugInAlgorithmCompleteCatalog(final File file, final FileWriter out/*,final int lines*/) {
        super();
        catalogFile = file;
        csv = out;
        //remove = lines;
    }

    @Override
    public void runAlgorithm() {
        // TODO Auto-generated method stub
        complete = new int[6]; // RNA, Plasma, Serum, CSF, Blood, DNA
        final String[] types = {"RNA", "Plasma", "Serum", "CSF", "Blood", "DNA"};
        String GUIDStr;
        Integer typeNum;

        readCSV();
        
        while (catalogID.size() > 0) {
            GUIDStr = catalogID.get(0);
            typeNum = type.get(0);
            if (compare(GUIDStr, typeNum)) {
                complete[typeNum.intValue()]++;
            }
        }

        try {
            csv.append("Sample Type,Number of Complete Sets\n");
            for (int i = 0; i < 6; i++) {
                csv.append(types[i] + "," + String.valueOf(complete[i]) + "\n");
            }
            csv.flush();
            csv.close();

        } catch (final IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        
        setCompleted(true);

    }

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
        if(IDvisit.size()<2) return false;
        for (int i = 0; i < IDvisit.size(); i++) {
            visitType = IDvisit.get(i).split(" ");
            if (visitType.length == 2) {
                month = Integer.parseInt(visitType[0]);
                if (month > max) {
                    max = month;
                }
            }
        }
        if(which.equals(new Integer(3)))
        	num = max / 12 + 1;
        else
        	num = max / 6 + 1;
        return (num == IDvisit.size());
    }

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

                /*for (int i = 0; i < remove; i++) {
                    input.readLine();
                }*/
                
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
