package gov.nih.mipav.model.file;


import java.util.*;


/**
 * DOCUMENT ME!
 */
public class FilenameSorter {

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * This method determines whether two String objects have the same characters up to the beginning index of the last
     * numerical sequence of numerals.
     *
     * @param   str1  DOCUMENT ME!
     * @param   str2  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public static boolean commonSet(String str1, String str2) {
        String substring1 = str1.substring(0, lastNumericalSequenceBegin(str1));
        String substring2 = str2.substring(0, lastNumericalSequenceBegin(str2));

        if (substring1.equals(substring2)) {
            return true;
        }

        return false;
    }

    /**
     * The purpose of this method is to compare two filename strings on the basis of their last numerical sequence. For
     * example, the filenames "dicom84.dcm" and "dicom83.dcm" will be compared to each other on the numbers 83 and 84.
     *
     * @param   str1  String
     * @param   str2  String
     *
     * @return  int
     */
    public static int compareToLastNumericalSequence(String str1, String str2) {
        String substring1;
        String substring2;

        int begin; // begin index of last numerical sequence
        int end; // ending index of last numerical sequence

        end = lastNumericalSequenceEnd(str1);
        begin = lastNumericalSequenceBegin(str1, end);

        if ((end < 0) || (begin < 0)) {
            return 1; // if this happens, no number is in the filename, default to greater than for str1
        }

        substring1 = str1.substring(begin, end + 1);

        end = lastNumericalSequenceEnd(str2);
        begin = lastNumericalSequenceBegin(str2, end);

        if ((end < 0) || (begin < 0)) {
            return -1; // if this happens, no number is in the filename, default to less than for str2
        }

        substring2 = str2.substring(begin, end + 1);

        Long long1 = new Long(Long.parseLong(substring1));
        Long long2 = new Long(Long.parseLong(substring2));

        return long1.compareTo(long2);
    }

    /**
     * This method will take a String array parameter and break it into a Vector of Vectors. The encompassing Vector
     * will hold references to Vectors of String, which should represent a set of related data. The related data is
     * meant to have the same characters up until the beginning index of the last numerical sequence of numerals.
     *
     * @param   filenames  String[]
     *
     * @return  Vector
     */
    public static Vector extractSubSets(String[] filenames) {
        Arrays.sort(filenames);

        Vector allSetsVector = new Vector();
        Vector setVector = new Vector();

        for (int i = 0; i < filenames.length; i++) {
            setVector.addElement(filenames[i]);

            if ((i + 1) >= filenames.length) {
                allSetsVector.addElement(setVector);

                break;
            }

            if (commonSet(filenames[i], filenames[i + 1])) {
                continue;
            }

            allSetsVector.addElement(setVector);
            setVector = new Vector();
        }

        return allSetsVector;
    }

    /**
     * Alternative to lastNumericalSequenceBegin(String, int), this method should be called if you don't already have
     * the index of the end of the last numerical sequence in the String parameter.
     *
     * @param   str  String
     *
     * @return  int
     */
    public static int lastNumericalSequenceBegin(String str) {
        int lastNumSequenceEnd = lastNumericalSequenceEnd(str);

        return lastNumericalSequenceBegin(str, lastNumSequenceEnd);
    }

    /**
     * The purpose of this method is to find the index location of a String that is the beginning of the last numerical
     * sequence. For example, when passed the string "dicom84.dcm" the method will return 5.
     *
     * @param   str                 String
     * @param   lastNumSequenceEnd  int
     *
     * @return  int
     */
    public static int lastNumericalSequenceBegin(String str, int lastNumSequenceEnd) {

        if (lastNumSequenceEnd == 0) {
            return 0;
        }

        for (int i = lastNumSequenceEnd; i > 0; i--) {

            if (!Character.isDigit(str.charAt(i - 1))) {
                return i;
            }
        }

        return 0;
    }

    /**
     * The purpose of this method is to find the index location of a String that is the end of the last numerical
     * sequence. For example, when passed the string "dicom84.dcm" this method will return 6.
     *
     * @param   str  String
     *
     * @return  int
     */
    public static int lastNumericalSequenceEnd(String str) {

        for (int i = str.length() - 1; i >= 0; i--) {

            if (Character.isDigit(str.charAt(i))) {
                return i;
            }
        }

        return -1;
    }

    /**
     * This method takes a sorted Vector of Vectors as a parameter. The encompassing Vector holds references to Vectors
     * of a particular set of Strings. These sets are sorted based on their last numerical sequence of numerals. The
     * rearranged Vector is then returned.
     *
     * @param   vector  Vector
     *
     * @return  Vector
     */
    public static Vector secondarySort(Vector vector) {

        for (int h = 0; h < vector.size(); h++) {
            Vector setVector = (Vector) vector.elementAt(h);

            for (int i = 0; i < setVector.size(); i++) {

                for (int j = i + 1; j < setVector.size(); j++) {
                    String str1 = (String) setVector.elementAt(i);
                    String str2 = (String) setVector.elementAt(j);

                    int result = compareToLastNumericalSequence(str1, str2); // compare based on last numerical sequence

                    if (result > 0) {
                        String fileTemp = str1;
                        str1 = str2;
                        str2 = fileTemp;

                        setVector.setElementAt(str1, i);
                        setVector.setElementAt(str2, j);
                    }
                }
            }
        }

        return vector;
    }

    /**
     * Method will convert a Vector of Vectors of String into a String array.
     *
     * @param   vector  Vector
     *
     * @return  String[]
     */
    public static String[] subSetsToArray(Vector vector) {
        int arraySize = 0;

        for (int i = 0; i < vector.size(); i++) {
            Vector subSet = (Vector) vector.elementAt(i);

            arraySize += subSet.size();
        }

        String[] arrayList = new String[arraySize];
        int index = 0;

        for (int i = 0; i < vector.size(); i++) {
            Vector subSet = (Vector) vector.elementAt(i);

            for (int j = 0; j < subSet.size(); j++) {
                arrayList[index] = (String) subSet.elementAt(j);
                index++;
            }
        }

        return arrayList;
    }
}
