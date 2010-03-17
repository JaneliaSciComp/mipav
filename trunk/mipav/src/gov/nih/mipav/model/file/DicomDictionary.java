package gov.nih.mipav.model.file;


import gov.nih.mipav.view.GetPath;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.text.DateFormat;
import java.util.Calendar;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.NoSuchElementException;
import java.util.StringTokenizer;


/**
 * This class reads a text file that lists all the tags that the DICOM file reader will need to know about. It stores
 * these in a static Hashtable. We assume the dictionary file is found in the working directory of mipav or in the root
 * of the jar file MIPAV is being run from (see the ClassLoader.getResource() method, with the default name
 * &quot;dicom_dictionary.txt&quot;.
 * 
 * <p>
 * The dictionary defines each tag on its own line, begining with the tag's ID in parenthesis, for example: <code>
 * (01AC,1101)</code>,
 * where the initial 4 digits are the group number, and the second for digits are the element number. Following the
 * group and element, each attribute of the tag definition is specified by attribute type, followed by an &quot;=&quot;,
 * then the attribute value in quotations. The attributes are separated by tabs, and are held in order:
 * </p>
 * 
 * <ol>
 * <li>VERS (DICOM version)</li>
 * <li>VR (Value Representation)</li>
 * <li>VM (Value Multiplicity)</li>
 * <li>KEYWORD (the NAME, without white-space)</li>
 * <li>NAME (A real-world description of the meaning of this tag)</li>
 * </ol>
 * 
 * <p>
 * Once the dictionary is parsed, the tag values are blank, so that when FileDicom reads the new file, it sets the
 * Hashtable in FileInfoDicom to this Hashtable and then sets the value attributes as it reads them in. This way all
 * standard tags are accounted for and if their value attribute is null, the FileInfoDicom table will not display that
 * tag.
 * </p>
 * 
 * <p>
 * Furthermore, the utility of this class is enhanced to provide two additional functions:
 * </p>
 * 
 * <ul>
 * <li>It parses a dictionary file</li>
 * <li>It has a static method to write out a dicom dictionary out to a file.</li>
 * </ul>
 * 
 * @version 1.0 Aug 1, 1999
 * @author Neva Cherniavsky
 * @see #parseFile
 * @see FileInfoDicom
 * @see FileDicom
 * @see FileDicomInfo
 */
public class DicomDictionary {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -8388829175738482222L;

    /** default dictionary file name, &quot;dicom_dictionary.txt&quot;. */
    public static final String DEFAULT_DICTIONARY_FILENAME = "dicom_dictionary.txt";

    /** subset dictionary file name, &quot;dicomsave.dictionary&quot;. */
    public static final String SUBSET_DICTIONARY_FILENAME = "dicomsave.dictionary";

    /** DOCUMENT ME! */
    private static final int DEFAULT_DICTIONARY = 1;

    /** DOCUMENT ME! */
    private static final int SUBSET_DICTIONARY = 2;

    /** Hashtable filled with known DICOM tags with empty value attributes. */
    private static Hashtable<FileDicomKey, FileDicomTagInfo> masterHashtable;

    /**
     * Hashtable filled with DICOM tags which are a subset (not necessarily a proper subset) of dicom tags in the master
     * table. This subset is then used to export dicom tags to the XML image format.
     */
    private static Hashtable<FileDicomKey, FileDicomTagInfo> subsetHashtable;

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Returns whether the dicom dictionary contains a tag with the given key identifier.
     * 
     * @param key the key for this tag
     * 
     * @return whether a tag matching the given key is contained in the dicom dictionary.
     */
    public static boolean containsTag(FileDicomKey key) {

        if (masterHashtable == null) {
            parseFile(DEFAULT_DICTIONARY);
        }
        
        //if this key is one of a series, it needs to be converted to wild card chars for the dictionary
        key = convertToWildKey(key);
        
        return masterHashtable.containsKey(key);
    }

    /**
     * Returns a reference to the DICOM Hashtable.
     * 
     * @return a reference to the dicom tag table
     */
    public static Hashtable<FileDicomKey, FileDicomTagInfo> getDicomTagTable() {
        return getDicomTagTable(false);
    }

    /**
     * Returns a reference to the DICOM Hashtable.
     * 
     * @param forceReload If true, forces the master tag table to be re-read from the dicom dictionary file
     * 
     * @return a reference to the dicom tag table
     */
    public static Hashtable<FileDicomKey, FileDicomTagInfo> getDicomTagTable(boolean forceReload) {

        if ( (masterHashtable == null) || (forceReload == true)) {
            parseFile(DEFAULT_DICTIONARY);
        }

        Hashtable<FileDicomKey, FileDicomTagInfo> clonedHashtable = new Hashtable<FileDicomKey, FileDicomTagInfo>(
                (int) (masterHashtable.size() / 0.7));
        Enumeration<FileDicomKey> e = masterHashtable.keys();

        while (e.hasMoreElements()) {
            FileDicomKey key = e.nextElement();
            FileDicomTagInfo value = (FileDicomTagInfo) masterHashtable.get(key).clone();

            clonedHashtable.put(key, value);
        }

        return clonedHashtable;
    }

    /**
     * Return information about a key in the dicom dictionary.
     * 
     * @param key the key to retreive information about
     * 
     * @return information about the requested key
     */
    public static FileDicomTagInfo getInfo(FileDicomKey key) {

        if (masterHashtable == null) {
            parseFile(DEFAULT_DICTIONARY);
        }
        
        //if this key is one of a series, it needs to be converted to wild card chars for the dictionary
        key = convertToWildKey(key);

        return masterHashtable.get(key);
    }

    /**
     * Find the key of the corresponding tag name in the hashtable. Returns null if it no key is found.
     * 
     * @param searchTagName The name of the tag for which you want the key.
     * 
     * @return The key as a String, for example "0010,0028" If the tag name is not in the hashtable, <code>null</code>
     *         is returned.
     */
    public static String getKeyFromTagName(String searchTagName) {
        Enumeration<FileDicomKey> enumeration = masterHashtable.keys();

        while (enumeration.hasMoreElements()) {
            FileDicomKey key = enumeration.nextElement();

            //if this key is one of a series, it needs to be converted to wild card chars for the dictionary
            FileDicomKey searchKey = convertToWildKey(key);
            
            FileDicomTagInfo tag = masterHashtable.get(searchKey);

            String foundTagName = tag.getName();

            if (foundTagName.equals(searchTagName)) {
                return key.getKey();
            }
        }

        return null;
    }

    /**
     * Find the keyword of the tag given by the key in the hashtable.
     * 
     * @param key the key of the desired name.
     * 
     * @return the keyword (the 2nd-to-last entry in &quot;dicom_dictionary.txt&quot;). if the tag is not in the
     *         hashtable, <code>null</code> is returned.
     */
    public static String getKeyword(FileDicomKey key) {

        if (masterHashtable == null) {
            parseFile(DEFAULT_DICTIONARY);
        }

        //if this key is one of a series, it needs to be converted to wild card chars for the dictionary
        key = convertToWildKey(key);
        
        FileDicomTagInfo tag = masterHashtable.get(key);

        if (tag == null) {
            return null;
        }

        return tag.getKeyword();
    }

    /**
     * Find the realworld name of the tag given by the key in the hashtable.
     * 
     * @param key the key of the desired name.
     * 
     * @return the realworld name (the last enry in &quot;dicom_dictionary.txt&quot;). if the tag is not in the
     *         hashtable, <code>null</code> is returned.
     */
    public static String getName(FileDicomKey key) {

        if (masterHashtable == null) {
            parseFile(DEFAULT_DICTIONARY);
        }

        //if this key is one of a series, it needs to be converted to wild card chars for the dictionary
        key = convertToWildKey(key);

        FileDicomTagInfo tag = masterHashtable.get(key);
        
        if (tag == null) {
            return null;
        }

        return tag.getName();
    }

    /**
     * Quietly checks to see if the subset dicom dictionary exists on disk. Can be used to avoid a call to
     * getSubsetDicomTagTable() that might cause a warning dialog to appear.
     * 
     * @return True if {@link SUBSET_DICTIONARY_FILENAME} exists.
     */
    public static boolean doesSubsetDicomTagTableExist() {
        BufferedReader dictionaryReference = getFileReader(SUBSET_DICTIONARY_FILENAME);

        if (dictionaryReference == null) {
            Preferences.debug("Failed to read DICOM dictionary file from " + SUBSET_DICTIONARY_FILENAME);

            return false;
        }

        return true;
    }

    /**
     * Returns a reference to the subset dicom tag table.
     * 
     * @return A reference to the subset dicom tag table
     */
    public static Hashtable<FileDicomKey, FileDicomTagInfo> getSubsetDicomTagTable() {
        return getSubsetDicomTagTable(false);
    }

    /**
     * Returns a reference to the subset dicom tag table.
     * 
     * @param forceReload If true, forces the subset tag table to be re-read from the dicom save dictionary file
     * 
     * @return A reference to the subset dicom tag table
     */
    public static Hashtable<FileDicomKey, FileDicomTagInfo> getSubsetDicomTagTable(boolean forceReload) {

        if ( (subsetHashtable == null) || (forceReload == true)) {
            parseFile(SUBSET_DICTIONARY);
        }

        return subsetHashtable;
    }

    /**
     * Accessor that returns the type of the tag (different from, but related to the vr).
     * 
     * @param key the key of the desired name.
     * 
     * @return the type if the tag is not in the hashtable, <code>null</code> is returned.
     */
    public static String getType(FileDicomKey key) {

        if (masterHashtable == null) {
            parseFile(DEFAULT_DICTIONARY);
        }

        //if this key is one of a series, it needs to be converted to wild card chars for the dictionary
        key = convertToWildKey(key);
        
        FileDicomTagInfo tag = (FileDicomTagInfo) masterHashtable.get(key);

        if (tag == null) {
            return null;
        }

        return tag.getType();
    }

    /**
     * Accessor that returns the value multiplicity of the tag. The value multiplicity is how many instances of this
     * value representation (VR) there can be in one tag.
     * 
     * @param key the key of the desired name.
     * 
     * @return the value multiplicity if the tag is not in the hashtable, <code>0</code> is returned.
     */
    public static int getVM(FileDicomKey key) {

        if (masterHashtable == null) {
            parseFile(DEFAULT_DICTIONARY);
        }

        //if this key is one of a series, it needs to be converted to wild card chars for the dictionary
        key = convertToWildKey(key);
        
        FileDicomTagInfo tag = (FileDicomTagInfo) masterHashtable.get(key);

        if (tag == null) {
            return 0;
        }

        return tag.getValueMultiplicity();
    }

    /**
     * Accessor that returns the value representation of the tag. The value representation allows the reader the read
     * and interpret the tag properly. Because private tags are not unique, the VR is null and they may be read and/or
     * displayed improperly.
     * 
     * @param key the key of the desired name.
     * 
     * @return the value representation if the tag is not in the hashtable, <code>null</code> is returned.
     */
    public static String getVR(FileDicomKey key) {

        if (masterHashtable == null) {
            parseFile(DEFAULT_DICTIONARY);
        }

        //if this key is one of a series, it needs to be converted to wild card chars for the dictionary
        key = convertToWildKey(key);
        
        FileDicomTagInfo tag = (FileDicomTagInfo) masterHashtable.get(key);

        if (tag == null) {
            return null;
        }

        return tag.getValueRepresentation();
    }

    /**
     * Sorts the list of tags using a slightly modified shell-sort and returns it as an array in order of FileDicomKeys.
     * This sorting routine is taken from <u>Numerical Recipes in C</u>, 2nd ed. by William H. Press, et al, page 332.
     * 
     * @param dicomTagsList The hashtable of DICOM tags (the keys are FileDicomKey and the object referred to by that
     *            key is the FileDicomTagInfo).
     * 
     * @return a sorted array of DICOM Keys.
     */
    public static FileDicomKey[] sortTagKeys(Hashtable<FileDicomKey, FileDicomTagInfo> dicomTagsList) {
        FileDicomKey[] dicomKeys;

        dicomKeys = new FileDicomKey[dicomTagsList.size()];

        int q = 0;
        Enumeration<FileDicomKey> e = dicomTagsList.keys();

        while (e.hasMoreElements()) // collect list of keys we will consider

        // valid to sort:
        {
            dicomKeys[q] = (FileDicomKey) e.nextElement();
            q++;
        }

        // sort the list of keys (via shell sort):
        int numDICOMKeys = dicomKeys.length - 1;
        int inc = 1;
        FileDicomKey val;

        do {
            inc *= 3;
            inc++;
        } while (inc <= numDICOMKeys);

        do {
            inc /= 3;

            for (int i = inc; i <= numDICOMKeys; i++) {

                // orders all from index 0 to numDICOMKeys
                val = dicomKeys[i];

                int j = i;

                /*
                 * use the group and element numbers, gathering them in a way to accommodate the occasional general
                 * group/element number (like 60xx).
                 */
                int tempGN, valGN;

                try {
                    tempGN = dicomKeys[j - inc].getGroupNumber();
                } catch (NumberFormatException nfe) {
                    tempGN = Integer.parseInt(dicomKeys[j - inc].getGroup().replace('x', '0'), 0x10);
                }

                try {
                    valGN = val.getGroupNumber();
                } catch (NumberFormatException nfe) {
                    valGN = Integer.parseInt(val.getGroup().replace('x', '0'), 0x10);
                }

                int tempEN, valEN;

                try {
                    tempEN = dicomKeys[j - inc].getElementNumber();
                } catch (NumberFormatException nfe) {
                    tempEN = Integer.parseInt(dicomKeys[j - inc].getElement().replace('x', '0'), 0x10);
                }

                try {
                    valEN = val.getElementNumber();
                } catch (NumberFormatException nfe) {
                    valEN = Integer.parseInt(val.getElement().replace('x', '0'), 0x10);
                }

                // check on both group and element numbers at the same time:
                while ( (tempGN > valGN) || ( (tempGN == valGN) && (tempEN > valEN))) {
                    dicomKeys[j] = dicomKeys[j - inc];
                    j -= inc;

                    if (j <= inc) {
                        break;
                    }

                    // else, reset the temp GN & temp EN
                    try {
                        tempGN = dicomKeys[j - inc].getGroupNumber();
                    } catch (NumberFormatException nfe) {
                        tempGN = Integer.parseInt(dicomKeys[j - inc].getGroup().replace('x', '0'), 0x10);
                    }

                    try {
                        tempEN = dicomKeys[j - inc].getElementNumber();
                    } catch (NumberFormatException nfe) {
                        tempEN = Integer.parseInt(dicomKeys[j - inc].getElement().replace('x', '0'), 0x10);
                    }
                }

                dicomKeys[j] = val;
            }
        } while (inc > 1);

        return dicomKeys;
    }

    /**
     * Writes the DICOMHashtable given to the file that the CreateDicomFiles points to.
     * 
     * @param dictFile the dicom dictionary file to write out
     * @param dicomHash the dicom tag mapping to write out
     * 
     * @throws IOException when the file cannot be written to.
     */
    public static void writeFile(File dictFile, Hashtable<FileDicomKey, FileDicomTagInfo> dicomHash) throws IOException {
        writeFile(dictFile, dicomHash, null);
    }

    /**
     * Writes the DICOMHashtable given to the file that the CreateDicomFiles points to.
     * 
     * @param dictFile the dicom dictionary file to write out
     * @param dicomHash the dicom tag mapping to write out
     * @param altComment an additional comment to write out to the file
     * 
     * @throws IOException when the file cannot be written to.
     */
    public static void writeFile(File dictFile, Hashtable<FileDicomKey, FileDicomTagInfo> dicomHash, String altComment)
            throws IOException {

        if ( !dictFile.canWrite()) {
            throw new IOException(dictFile + " cannot be written.");
        }

        FileWriter fw = new FileWriter(dictFile, false);

        FileDicomKey[] sortedKeys = DicomDictionary.sortTagKeys(dicomHash);

        Calendar c = Calendar.getInstance();
        DateFormat df = DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.MEDIUM);

        fw.write(" # DICOM Tag Dictionary \n");
        fw.write(" # " + df.format(c.getTime()) + "\n");
        fw.write(" # DICOM definition formatting:\n");
        fw.write(" #    ([group ID],[element ID])\n");
        fw.write(" #    VERS=\"[version value]\"\n");
        fw.write(" #    VR=\"[value representation]\"\n");
        fw.write(" #    VM=\"[value multiplicity, numeric value or \"1-n\"]\n");
        fw.write(" #    Keyword=\"[alphanumeric string for tag's keyword]\"\n");
        fw.write(" #    Name=\"[Name of Tag]\"\n");

        if (altComment != null) {
            fw.write(" # " + altComment + "\n");
        }

        for (int i = 0; i < sortedKeys.length; i++) {
            FileDicomTagInfo tag = ((FileDicomTagInfo) dicomHash.get(sortedKeys[i]));
            fw.write("(" + sortedKeys[i].toString() + ")\t");
            fw.write("VERS=\"" + tag.getVersion() + "\"\t");
            fw.write("VR=\"" + tag.getValueRepresentation() + "\"\t");
            fw.write("VM=\"" + tag.getValueMultiplicity() + "\"\t");
            fw.write("Keyword=\"" + tag.getKeyword() + "\"\t");
            fw.write("Name=\"" + tag.getName() + "\"\n");
        }

        fw.write(" # End of Tag Dictionary file.\n");
        fw.close();
    }
    
    /**
     * Converts group numbers of 60xx or 50xx dicom key elements so that the 
     * dicom dictionary will be able to find them.
     */
    
    private static FileDicomKey convertToWildKey(FileDicomKey key) {
        String wildCheck = key.getGroup().substring(0, 2);
        //if key group is not a 50xx or 60xx, then returning the masterHashtable evaluation is enough
        if(!wildCheck.equals("50") && !wildCheck.equals("60")) {
            return key;
        } else { //dicom dictionary stores wildcard values, so check after converting group name
            String keyStr = key.toString();
            int commaLoc = keyStr.indexOf(',');
            keyStr = keyStr.substring(0, commaLoc-2)+"xx"+keyStr.substring(commaLoc);
            FileDicomKey newKey = (FileDicomKey) key.clone();
            newKey.setKey(keyStr);
            return newKey;
        }
    }

    /**
     * Gets a buffered reader for a given file name.
     * 
     * @param filename The file we will be reading.
     * 
     * @return A reader for the given file name.
     */
    private static BufferedReader getFileReader(String filename) {

        try {
            String filepath;

            if (filename.equals(DEFAULT_DICTIONARY_FILENAME)) {
                URL fileURL = Thread.currentThread().getContextClassLoader().getResource(filename);

                return new BufferedReader(new InputStreamReader(fileURL.openStream()));
            } else {
                filepath = GetPath.getPath(filename, GetPath.FOR_READING);
            }

            if (filepath == null) {
                filepath = "";
            }

            File dictionaryFile = new File(filepath + filename);

            if ( !dictionaryFile.exists()) {
                throw new FileNotFoundException(dictionaryFile.getAbsolutePath() + " does not exist.");
            }

            if ( !dictionaryFile.isFile()) {
                throw new FileNotFoundException(dictionaryFile.getAbsolutePath() + " is not a file.");
            }

            if ( !dictionaryFile.canRead()) {
                throw new FileNotFoundException(dictionaryFile.getAbsolutePath() + " does not have 'read' permissions.");
            }

            return new BufferedReader(new FileReader(dictionaryFile));
        } catch (Throwable t) {
            return null;
        }
    }

    /**
     * Method called once when the user opens MIPAV. It parses the dictionary file, normally called
     * &quot;dicom_dictionary.txt&quot;. The dictionary file is where all the tags are listed and stores these in the
     * DICOMHashtable, with empty value attributes.
     * 
     * @see FileDicomTagInfo
     */
    private static void parseFile(int dictionary_type) {
        String filename;
        Hashtable<FileDicomKey, FileDicomTagInfo> hashtable = new Hashtable<FileDicomKey, FileDicomTagInfo>();

        if (dictionary_type == SUBSET_DICTIONARY) {
            filename = SUBSET_DICTIONARY_FILENAME;
        } else {
            filename = DEFAULT_DICTIONARY_FILENAME;
        }

        BufferedReader dictionaryReference = getFileReader(filename);

        if (dictionaryReference == null) {
            Preferences.debug("Failed to read DICOM dictionary file from " + filename);
            MipavUtil.displayWarning("Failed to read DICOM dictionary file from " + filename);

            return;
        }

        FileDicomKey key;

        try {
            String s;

            while ( (s = dictionaryReference.readLine()) != null) {
                s = s.trim();

                try {

                    if (s.charAt(0) == '#') { // indicates a comment
                        continue;
                    }
                } catch (StringIndexOutOfBoundsException str) {
                    // String index out of bounds on s.charAt(0) probably
                    // means the string is empty.

                    continue;
                }

                StringTokenizer tok = new StringTokenizer(s, "=");
                String values = "";

                try {
                    values = ((String) tok.nextElement()).trim();
                } catch (NoSuchElementException noway) {
                    continue;
                }

                // Key is the hash key and can have values 60xx where xx yet undefined.
                key = new FileDicomKey(values.substring(1, 10));

                if ( !tok.hasMoreElements()) {

                    // so we will ignore additional information about the tag
                    // but will instead
                    continue;
                }

                values = (String) tok.nextElement();

                int index = values.lastIndexOf("\"");
                String vers = values.substring(1, index);

                values = (String) tok.nextElement();

                String vr = values.substring(1, 5);

                if ( !vr.equals("NONE")) {
                    vr = vr.substring(0, 2);
                }

                values = (String) tok.nextElement();
                index = values.lastIndexOf("\"");

                String vmS = values.substring(1, index);
                int vm;

                try {
                    vm = Integer.valueOf(vmS).intValue();
                } catch (NumberFormatException e) {

                    if (vmS.endsWith("n")) {
                        vm = Integer.MAX_VALUE;
                    } else {
                        vm = Integer.valueOf(vmS.substring(2)).intValue();
                    }
                }

                if (vers.equals("2") || vers.equals("3")) {
                    values = (String) tok.nextElement();
                    index = values.lastIndexOf("\"");

                    String keyword = values.substring(1, index);

                    values = (String) tok.nextElement();
                    index = values.lastIndexOf("\"");

                    String name = values.substring(1, index);

                    hashtable.put(key, new FileDicomTagInfo(key, vers, vr, vm, keyword, name));
                } else {

                    if (values.lastIndexOf("Keyword") == -1) {
                        values = (String) tok.nextElement();
                        values = (String) tok.nextElement();
                        index = values.lastIndexOf("\"");

                        String keyword = values.substring(1, index);

                        values = (String) tok.nextElement();
                        index = values.lastIndexOf("\"");

                        String name = values.substring(1, index);
                        hashtable.put(key, new FileDicomTagInfo(key, vers, vr, vm, keyword, name));
                    } else {
                        values = (String) tok.nextElement();
                        index = values.lastIndexOf("\"");

                        String keyword = values.substring(1, index);

                        values = (String) tok.nextElement();
                        index = values.lastIndexOf("\"");

                        String name = values.substring(1, index);
                        hashtable.put(key, new FileDicomTagInfo(key, vers, vr, vm, keyword, name));
                    }
                }
            }
        } catch (FileNotFoundException fnfe) {
            Preferences.debug("Dictionary file not found: " + filename);
            MipavUtil.displayWarning("Dictionary file not found: " + filename);
        } catch (Exception e) {
            MipavUtil.displayError("Failed to parse DICOM dictionary file.");
            e.printStackTrace();
        } finally {

            try {

                if (dictionaryReference != null) {
                    dictionaryReference.close();
                    dictionaryReference = null;
                }
            } catch (IOException closee) {
                // ignore a problem closing the dictionary
            }

            if (dictionary_type == SUBSET_DICTIONARY) {
                subsetHashtable = hashtable;
            } else {
                masterHashtable = hashtable;
            }
        }
    }
}
