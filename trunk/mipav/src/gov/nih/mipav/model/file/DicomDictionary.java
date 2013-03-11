package gov.nih.mipav.model.file;


import gov.nih.mipav.model.file.FileDicomTagInfo.VR;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.GetPath.Purpose;

import java.io.*;
import java.net.URL;
import java.text.DateFormat;
import java.util.*;


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
    protected static final int DEFAULT_DICTIONARY = 1;

    /** DOCUMENT ME! */
    protected static final int SUBSET_DICTIONARY = 2;

    /** Hashtable filled with known DICOM tags with empty value attributes. */
    protected static Hashtable<FileDicomKey, FileDicomTagInfo> masterHashtable;

    /**
     * Hashtable filled with DICOM tags which are a subset (not necessarily a proper subset) of dicom tags in the master
     * table. This subset is then used to export dicom tags to the XML image format.
     */
    protected static Hashtable<FileDicomKey, FileDicomTagInfo> subsetHashtable;

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

        if (DicomDictionary.masterHashtable == null) {
            DicomDictionary.parseFile(DicomDictionary.DEFAULT_DICTIONARY);
        }

        // if this key is one of a series, it needs to be converted to wild card chars for the dictionary
        key = DicomDictionary.convertToWildKey(key);

        return DicomDictionary.masterHashtable.containsKey(key);
    }

    /**
     * Returns a reference to the DICOM Hashtable.
     * 
     * @return a reference to the dicom tag table
     */
    public static Hashtable<FileDicomKey, FileDicomTagInfo> getDicomTagTable() {
        return DicomDictionary.getDicomTagTable(false);
    }

    /**
     * Returns a reference to the DICOM Hashtable.
     * 
     * @param forceReload If true, forces the master tag table to be re-read from the dicom dictionary file
     * 
     * @return a reference to the dicom tag table
     */
    public static Hashtable<FileDicomKey, FileDicomTagInfo> getDicomTagTable(final boolean forceReload) {

        if ( (DicomDictionary.masterHashtable == null) || (forceReload == true)) {
            DicomDictionary.parseFile(DicomDictionary.DEFAULT_DICTIONARY);
        }

        final Hashtable<FileDicomKey, FileDicomTagInfo> clonedHashtable = new Hashtable<FileDicomKey, FileDicomTagInfo>(
                (int) (DicomDictionary.masterHashtable.size() / 0.7));
        final Enumeration<FileDicomKey> e = DicomDictionary.masterHashtable.keys();

        while (e.hasMoreElements()) {
            final FileDicomKey key = e.nextElement();
            final FileDicomTagInfo value = (FileDicomTagInfo) DicomDictionary.masterHashtable.get(key).clone();

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

        if (DicomDictionary.masterHashtable == null) {
            DicomDictionary.parseFile(DicomDictionary.DEFAULT_DICTIONARY);
        }

        // if this key is one of a series, it needs to be converted to wild card chars for the dictionary
        key = DicomDictionary.convertToWildKey(key);

        return DicomDictionary.masterHashtable.get(key);
    }

    /**
     * Find the key of the corresponding tag name in the hashtable. Returns null if it no key is found.
     * 
     * @param searchTagName The name of the tag for which you want the key.
     * 
     * @return The key as a String, for example "0010,0028" If the tag name is not in the hashtable, <code>null</code>
     *         is returned.
     */
    public static String getKeyFromTagName(final String searchTagName) {
        final Enumeration<FileDicomKey> enumeration = DicomDictionary.masterHashtable.keys();

        while (enumeration.hasMoreElements()) {
            final FileDicomKey key = enumeration.nextElement();

            // if this key is one of a series, it needs to be converted to wild card chars for the dictionary
            final FileDicomKey searchKey = DicomDictionary.convertToWildKey(key);

            final FileDicomTagInfo tag = DicomDictionary.masterHashtable.get(searchKey);

            final String foundTagName = tag.getName();

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

        if (DicomDictionary.masterHashtable == null) {
            DicomDictionary.parseFile(DicomDictionary.DEFAULT_DICTIONARY);
        }

        // if this key is one of a series, it needs to be converted to wild card chars for the dictionary
        key = DicomDictionary.convertToWildKey(key);

        final FileDicomTagInfo tag = DicomDictionary.masterHashtable.get(key);

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

        if (DicomDictionary.masterHashtable == null) {
            DicomDictionary.parseFile(DicomDictionary.DEFAULT_DICTIONARY);
        }

        // if this key is one of a series, it needs to be converted to wild card chars for the dictionary
        key = DicomDictionary.convertToWildKey(key);

        final FileDicomTagInfo tag = DicomDictionary.masterHashtable.get(key);

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
        final BufferedReader dictionaryReference = DicomDictionary
                .getFileReader(DicomDictionary.SUBSET_DICTIONARY_FILENAME);

        if (dictionaryReference == null) {
            Preferences
                    .debug("Failed to read DICOM dictionary file from " + DicomDictionary.SUBSET_DICTIONARY_FILENAME);

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
        return DicomDictionary.getSubsetDicomTagTable(false);
    }

    /**
     * Returns a reference to the subset dicom tag table.
     * 
     * @param forceReload If true, forces the subset tag table to be re-read from the dicom save dictionary file
     * 
     * @return A reference to the subset dicom tag table
     */
    public static Hashtable<FileDicomKey, FileDicomTagInfo> getSubsetDicomTagTable(final boolean forceReload) {

        if ( (DicomDictionary.subsetHashtable == null) || (forceReload == true)) {
            DicomDictionary.parseFile(DicomDictionary.SUBSET_DICTIONARY);
        }

        return DicomDictionary.subsetHashtable;
    }

    /**
     * Accessor that returns the type of the tag (different from, but related to the vr).
     * 
     * @param key the key of the desired name.
     * 
     * @return the type if the tag is not in the hashtable, <code>null</code> is returned.
     */
    public static VR getType(FileDicomKey key) {

        if (DicomDictionary.masterHashtable == null) {
            DicomDictionary.parseFile(DicomDictionary.DEFAULT_DICTIONARY);
        }

        // if this key is one of a series, it needs to be converted to wild card chars for the dictionary
        key = DicomDictionary.convertToWildKey(key);

        final FileDicomTagInfo tag = DicomDictionary.masterHashtable.get(key);

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

        if (DicomDictionary.masterHashtable == null) {
            DicomDictionary.parseFile(DicomDictionary.DEFAULT_DICTIONARY);
        }

        // if this key is one of a series, it needs to be converted to wild card chars for the dictionary
        key = DicomDictionary.convertToWildKey(key);

        final FileDicomTagInfo tag = DicomDictionary.masterHashtable.get(key);

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
    public static VR getVR(FileDicomKey key) {

        if (DicomDictionary.masterHashtable == null) {
            DicomDictionary.parseFile(DicomDictionary.DEFAULT_DICTIONARY);
        }

        // if this key is one of a series, it needs to be converted to wild card chars for the dictionary
        key = DicomDictionary.convertToWildKey(key);

        final FileDicomTagInfo tag = DicomDictionary.masterHashtable.get(key);

        if (tag == null) {
            return null;
        }

        return tag.getValueRepresentation();
    }

    /**
     * Sorts the list of tags and returns it as an array in order of FileDicomKeys.
     * 
     * @param dicomTagsList The hashtable of DICOM tags (the keys are FileDicomKey and the object referred to by that
     *            key is the FileDicomTagInfo).
     * 
     * @return a sorted array of DICOM Keys.
     */
    public static FileDicomKey[] sortTagKeys(final Hashtable<FileDicomKey, FileDicomTagInfo> dicomTagsList) {
        FileDicomKey[] dicomKeys;

        dicomKeys = new FileDicomKey[dicomTagsList.size()];

        int q = 0;
        final Enumeration<FileDicomKey> e = dicomTagsList.keys();

        // collect list of keys we will consider valid to sort:
        while (e.hasMoreElements()) {
            dicomKeys[q] = e.nextElement();
            q++;
        }

        final Comparator<FileDicomKey> comparator = new Comparator<FileDicomKey>() {
            public int compare(final FileDicomKey key1, final FileDicomKey key2) {
                // use the group and element numbers, gathering them in a way to accommodate the occasional general
                // group/element number (like 60xx).
                int key1GN, key2GN;

                try {
                    key1GN = key1.getGroupNumber();
                } catch (final NumberFormatException nfe) {
                    key1GN = Integer.parseInt(key1.getGroup().replace('x', '0'), 0x10);
                }

                try {
                    key2GN = key2.getGroupNumber();
                } catch (final NumberFormatException nfe) {
                    key2GN = Integer.parseInt(key2.getGroup().replace('x', '0'), 0x10);
                }

                int key1EN, key2EN;

                try {
                    key1EN = key1.getElementNumber();
                } catch (final NumberFormatException nfe) {
                    key1EN = Integer.parseInt(key1.getElement().replace('x', '0'), 0x10);
                }

                try {
                    key2EN = key2.getElementNumber();
                } catch (final NumberFormatException nfe) {
                    key2EN = Integer.parseInt(key2.getElement().replace('x', '0'), 0x10);
                }

                if (key1GN < key2GN || (key1GN == key2GN && key1EN < key2EN)) {
                    return -1;
                } else if (key1GN == key2GN && key1EN == key2EN) {
                    return 0;
                } else {
                    return 1;
                }
            }
        };

        Arrays.sort(dicomKeys, comparator);

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
    public static void writeFile(final File dictFile, final Hashtable<FileDicomKey, FileDicomTagInfo> dicomHash)
            throws IOException {
        DicomDictionary.writeFile(dictFile, dicomHash, null);
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
    public static void writeFile(final File dictFile, final Hashtable<FileDicomKey, FileDicomTagInfo> dicomHash,
            final String altComment) throws IOException {

        if ( !dictFile.canWrite()) {
            throw new IOException(dictFile + " cannot be written.");
        }

        final FileWriter fw = new FileWriter(dictFile, false);

        final FileDicomKey[] sortedKeys = DicomDictionary.sortTagKeys(dicomHash);

        final Calendar c = Calendar.getInstance();
        final DateFormat df = DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.MEDIUM);

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

        for (final FileDicomKey element : sortedKeys) {
            final FileDicomTagInfo tag = (dicomHash.get(element));
            fw.write("(" + element.toString() + ")\t");
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
     * Converts group numbers of 60xx or 50xx dicom key elements so that the dicom dictionary will be able to find them.
     */

    protected static FileDicomKey convertToWildKey(final FileDicomKey key) {
        final String wildCheck = key.getGroup().substring(0, 2);
        // if key group is not a 50xx or 60xx, then returning the masterHashtable evaluation is enough
        if ( !wildCheck.equals("50") && !wildCheck.equals("60")) {
            return key;
        } else { // dicom dictionary stores wildcard values, so check after converting group name
            String keyStr = key.toString();
            final int commaLoc = keyStr.indexOf(',');
            keyStr = keyStr.substring(0, commaLoc - 2) + "xx" + keyStr.substring(commaLoc);
            final FileDicomKey newKey = (FileDicomKey) key.clone();
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
    protected static BufferedReader getFileReader(final String filename) {

        try {
            String filepath;

            if (filename.equals(DicomDictionary.DEFAULT_DICTIONARY_FILENAME)) {
                final URL fileURL = Thread.currentThread().getContextClassLoader().getResource(filename);

                return new BufferedReader(new InputStreamReader(fileURL.openStream()));
            } else {
                filepath = GetPath.getPath(filename, Purpose.FOR_READING);
            }

            if (filepath == null) {
                filepath = "";
            }

            final File dictionaryFile = new File(filepath + filename);

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
        } catch (final Throwable t) {
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
    protected static void parseFile(final int dictionary_type) {
        String filename;
        final Hashtable<FileDicomKey, FileDicomTagInfo> hashtable = new Hashtable<FileDicomKey, FileDicomTagInfo>();

        if (dictionary_type == DicomDictionary.SUBSET_DICTIONARY) {
            filename = DicomDictionary.SUBSET_DICTIONARY_FILENAME;
        } else {
            filename = DicomDictionary.DEFAULT_DICTIONARY_FILENAME;
        }

        BufferedReader dictionaryReference = DicomDictionary.getFileReader(filename);

        if (dictionaryReference == null) {
            Preferences.debug("Failed to read DICOM dictionary file from " + filename, Preferences.DEBUG_FILEIO);
            Preferences.debug("If saving a dicom image, no tags will be saved.", Preferences.DEBUG_FILEIO);
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
                } catch (final StringIndexOutOfBoundsException str) {
                    // String index out of bounds on s.charAt(0) probably
                    // means the string is empty.

                    continue;
                }

                final StringTokenizer tok = new StringTokenizer(s, "=");
                String values = "";

                try {
                    values = ((String) tok.nextElement()).trim();
                } catch (final NoSuchElementException noway) {
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
                final String vers = values.substring(1, index);

                values = (String) tok.nextElement();

                String vrStr = values.substring(1, 5);
                VR vr = VR.XX;

                if ( !vrStr.equals("NONE")) {
                    vrStr = vrStr.substring(0, 2);
                    vr = VR.valueOf(vrStr);
                } else {
                    vr = VR.UN;
                }

                values = (String) tok.nextElement();
                index = values.lastIndexOf("\"");

                final String vmS = values.substring(1, index);
                int vm;

                try {
                    vm = Integer.valueOf(vmS).intValue();
                } catch (final NumberFormatException e) {

                    if (vmS.endsWith("n")) {
                        vm = Integer.MAX_VALUE;
                    } else {
                        vm = Integer.valueOf(vmS.substring(2)).intValue();
                    }
                }

                if (vers.equals("2") || vers.equals("3")) {
                    values = (String) tok.nextElement();
                    index = values.lastIndexOf("\"");

                    final String keyword = values.substring(1, index);

                    values = (String) tok.nextElement();
                    index = values.lastIndexOf("\"");

                    final String name = values.substring(1, index);

                    hashtable.put(key, new FileDicomTagInfo(key, vers, vr, vm, keyword, name));
                } else {

                    if (values.lastIndexOf("Keyword") == -1) {
                        values = (String) tok.nextElement();
                        values = (String) tok.nextElement();
                        index = values.lastIndexOf("\"");

                        final String keyword = values.substring(1, index);

                        values = (String) tok.nextElement();
                        index = values.lastIndexOf("\"");

                        final String name = values.substring(1, index);
                        hashtable.put(key, new FileDicomTagInfo(key, vers, vr, vm, keyword, name));
                    } else {
                        values = (String) tok.nextElement();
                        index = values.lastIndexOf("\"");

                        final String keyword = values.substring(1, index);

                        values = (String) tok.nextElement();
                        index = values.lastIndexOf("\"");

                        final String name = values.substring(1, index);
                        hashtable.put(key, new FileDicomTagInfo(key, vers, vr, vm, keyword, name));
                    }
                }
            }
        } catch (final FileNotFoundException fnfe) {
            Preferences.debug("Dictionary file not found: " + filename, Preferences.DEBUG_FILEIO);
            MipavUtil.displayWarning("Dictionary file not found: " + filename);
        } catch (final Exception e) {
            MipavUtil.displayError("Failed to parse DICOM dictionary file.");
            e.printStackTrace();
        } finally {

            try {

                if (dictionaryReference != null) {
                    dictionaryReference.close();
                    dictionaryReference = null;
                }
            } catch (final IOException closee) {
                // ignore a problem closing the dictionary
            }

            if (dictionary_type == DicomDictionary.SUBSET_DICTIONARY) {
                DicomDictionary.subsetHashtable = hashtable;
            } else {
                DicomDictionary.masterHashtable = hashtable;
            }
        }
    }
}
