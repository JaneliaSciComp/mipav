import gov.nih.mipav.model.algorithms.AlgorithmBase;

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

    private final int remove;

    public PlugInAlgorithmCompleteCatalog(final File file, final FileWriter out, final int lines) {
        super();
        catalogFile = file;
        csv = out;
        remove = lines;
    }

    @Override
    public void runAlgorithm() {
        // TODO Auto-generated method stub
        complete = new int[5]; // RNA, Plasma, Serum, CSF, Blood
        final String[] types = {"RNA", "Plasma", "Serum", "CSF", "Blood"};
        String GUID;
        Integer typeNum;

        readCSV();
        while (catalogID.size() > 0) {
            GUID = catalogID.get(0);
            typeNum = type.get(0);
            if (compare(GUID, typeNum)) {
                complete[typeNum.intValue()]++;
            }
        }

        try {
            csv.append("Sample Type,Number of Complete Sets\n");
            for (int i = 0; i < 5; i++) {
                csv.append(types[i] + "," + String.valueOf(complete[i]) + "\n");
            }
            csv.flush();
            csv.close();
        } catch (final IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

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

                for (int i = 0; i < remove; i++) {
                    input.readLine();
                }

                while ( (line = input.readLine()) != null) {
                    lineArray = line.split(",");
                    if (lineArray.length != 0) {

                        typeStr = lineArray[6].trim().toLowerCase();

                        if (typeStr.equals("rna")) {
                            typeNum = 0;
                        } else if (typeStr.equals("plasma")) {
                            typeNum = 1;
                        } else if (typeStr.equals("serum")) {
                            typeNum = 2;
                        } else if (typeStr.equals("csf")) {
                            typeNum = 3;
                        } else if (typeStr.equals("blood")) {
                            typeNum = 4;
                        } else {
                            typeNum = -1;
                        }

                        if (typeNum != -1) {
                            type.add(new Integer(typeNum));
                            catalogID.add(lineArray[1].trim());
                            visit.add(lineArray[2].trim());
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
}
