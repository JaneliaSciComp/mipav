package gov.nih.mipav.model.file;

import java.io.*;
import java.util.*;
import java.text.*;
import java.net.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;

/**
 *
 * This class reads a text file that lists all the tags that the DICOM file
 * reader will need to know about. It stores these in a static Hashtable. We
 * assume the dictionary file is found in the working directory of mipav or in the
 * root of the jar file MIPAV is being run from (see the ClassLoader.getResource()
 * method, with the default name &quot;dicom_dictionary.txt&quot;.
 * <p>
 * <p>
 * The dictionary defines each tag on its own line, begining with the tag's ID
 * in parenthesis, for example: <code>(01AC,1101)</code>, where the initial 4
 * digits are the group number, and the second for digits are the element
 * number. Following the group and element, each attribute of the tag definition
 * is specified by attribute type, followed by an &quot;=&quot;, then the
 * attribute value in quotations. The attributes are separated by tabs, and are
 * held in order:
 * <ol>
 * <li>VERS (DICOM version)</li>
 * <li>VR (Value Representation)</li>
 * <li>VM (Value Multiplicity)</li>
 * <li>KEYWORD (the NAME, without white-space)</li>
 * <li>NAME (A real-world description of the meaning of this tag)</li>
 * </ol>
 *
 * <p>
 * Once the dictionary is parsed, the tag values are blank, so that when
 * FileDicom reads the new file, it sets the Hashtable in FileInfoDicom to this
 * Hashtable and then sets the value attributes as it reads them in. This way
 * all standard tags are accounted for and if their value attribute is null, the
 * FileInfoDicom table will not display that tag.
 * <p>
 * Furthermore, the utility of this class is enhanced to provide two additional
 * functions:
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
 * @see FileDicomTag
 */
public class DICOMDictionaryBuilder extends ModelSerialCloneable
{
    /** default dictionary file name, &quot;dicom_dictionary.txt&quot;. */
    public static final String DEFAULT_DICTIONARY_FILENAME = "dicom_dictionary.txt";

    /** subset dictionary file name, &quot;dicomsave.dictionary&quot;. */
    public static final String SUBSET_DICTIONARY_FILENAME = "dicomsave.dictionary";

    private static final int DEFAULT_DICTIONARY = 1;
    private static final int SUBSET_DICTIONARY = 2;

    /**
     * Default classname for the generated file being used in the Run Time
     * Class, which allows access to information in the DICOM data dictionary.
     * It is created for use with the dicomcomm package, a
     * UC-Berkeley-originated collection objects.
     */
    protected static final String defaultRTCclassName = "DICOM_RTC";

    /**
     * Default output file name for the RTC source output,
     * &quot;DICOM_RTC.java&quot;.
     */
    public static String default_RTC_name = defaultRTCclassName + ".java";

    /** Hashtable filled with known DICOM tags with empty value attributes. */
    private static Hashtable masterHashtable;
    private static Hashtable subsetHashtable;

    /**
     * Method called once when the user opens MIPAV. It parses the dictionary
     * file, normally called &quot;dicom_dictionary.txt&quot;. The dictionary
     * file is where all the tags are listed and stores these in the
     * DICOMHashtable, with empty value attributes.
     *
     * @see FileDicomTag
     */
    private static void parseFile(int dictionary_type)
    {
        String filename;
        Hashtable hashtable = new Hashtable();

        if (dictionary_type == SUBSET_DICTIONARY)
        {
            filename = SUBSET_DICTIONARY_FILENAME;
        }
        else
        {
            filename = DEFAULT_DICTIONARY_FILENAME;
        }

        BufferedReader dictionaryReference = getFileReader(filename);

        if (dictionaryReference == null)
        {
            Preferences.debug("Failed to read DICOM dictionary file from " + filename);
            MipavUtil.displayWarning("Failed to read DICOM dictionary file from " + filename);
            return;
        }

        FileDicomKey key;

        try
        {
            String s;
            while ( (s = dictionaryReference.readLine()) != null)
            {
                s = s.trim();
                try
                {
                    if (s.charAt(0) == '#')
                    { // indicates a comment
                        continue;
                    }
                }
                catch (StringIndexOutOfBoundsException str)
                {
                    // String index out of bounds on s.charAt(0) probably
                    // means the string is empty.

                    continue;
                }

                StringTokenizer tok = new StringTokenizer(s, "=");
                String values = "";
                try
                {
                    values = ( (String) tok.nextElement()).trim();
                }
                catch (NoSuchElementException noway)
                {
                    continue;
                }

                String temp = values.replace('x', '0');

                // Key is the hash key and can have values 60xx where xx yet
                // undefined.
                // However the tag defaults to 6000 until change in
                // FileInfoDicom.setvalue()
                key = new FileDicomKey(values.substring(1, 10));
                int group = Integer.valueOf(temp.substring(1, 5), 16).intValue();
                int element = Integer.valueOf(temp.substring(6, 10), 16).intValue();

                if (!tok.hasMoreElements())
                {
                    // so we will ignore additional information about the tag
                    // but will instead
                    continue;
                }

                values = (String) tok.nextElement();
                int index = values.lastIndexOf("\"");
                String vers = values.substring(1, index);

                values = (String) tok.nextElement();

                String vr = values.substring(1, 5);
                if (!vr.equals("NONE"))
                {
                    vr = vr.substring(0, 2);
                }

                values = (String) tok.nextElement();
                index = values.lastIndexOf("\"");
                String vmS = values.substring(1, index);
                int vm;
                try
                {
                    vm = Integer.valueOf(vmS).intValue();
                }
                catch (NumberFormatException e)
                {
                    if (vmS.endsWith("n"))
                    {
                        vm = Integer.MAX_VALUE;
                    }
                    else
                    {
                        vm = Integer.valueOf(vmS.substring(2)).intValue();
                    }
                }

                if (vers.equals("2") || vers.equals("3"))
                {
                    values = (String) tok.nextElement();
                    index = values.lastIndexOf("\"");
                    String keyword = values.substring(1, index);

                    values = (String) tok.nextElement();
                    index = values.lastIndexOf("\"");
                    String name = values.substring(1, index);

                    hashtable.put(key, new FileDicomTag(group, element, vr, vm, keyword, name));
                }
                else
                {
                    if (values.lastIndexOf("Keyword") == -1)
                    {
                        values = (String) tok.nextElement();
                        values = (String) tok.nextElement();
                        index = values.lastIndexOf("\"");
                        String keyword = values.substring(1, index);

                        values = (String) tok.nextElement();
                        index = values.lastIndexOf("\"");
                        String name = values.substring(1, index);
                        hashtable.put(key, new FileDicomTag(group, element, vr, vm, keyword, name));
                    }
                    else
                    {
                        values = (String) tok.nextElement();
                        index = values.lastIndexOf("\"");
                        String keyword = values.substring(1, index);

                        values = (String) tok.nextElement();
                        index = values.lastIndexOf("\"");
                        String name = values.substring(1, index);
                        hashtable.put(key, new FileDicomTag(group, element, vr, vm, keyword, name));
                    }
                }
            }
        }
        catch (FileNotFoundException fnfe)
        {
            Preferences.debug("Dictionary file not found: " + filename);
            MipavUtil.displayWarning("Dictionary file not found: " + filename);
        }
        catch (Exception e)
        {
            MipavUtil.displayError("Failed to parse DICOM dictionary file.");
            e.printStackTrace();
        }
        finally
        {
            try
            {
                if (dictionaryReference != null)
                {
                    dictionaryReference.close();
                    dictionaryReference = null;
                }
            }
            catch (IOException closee)
            {}

            if (dictionary_type == SUBSET_DICTIONARY)
            {
                subsetHashtable = hashtable;
            }
            else
            {
                masterHashtable = hashtable;
            }
        }

    }

    private static BufferedReader getFileReader(String filename)
    {
        try
        {
            String filepath;
            
            if (filename.equals(DEFAULT_DICTIONARY_FILENAME)) {
                URL fileURL = Thread.currentThread().getContextClassLoader().getResource(filename);
                return new BufferedReader( new InputStreamReader( fileURL.openStream() ) );
            } else {
                filepath = GetPath.getPath(filename, GetPath.FOR_READING);
            }

            if (filepath == null)
            {
                filepath = "";
            }

            File dictionaryFile = new File(filepath + filename);

            if (!dictionaryFile.exists())
            {
                throw new FileNotFoundException(dictionaryFile.getAbsolutePath() + " does not exist.");
            }
            if (!dictionaryFile.isFile())
            {
                throw new FileNotFoundException(dictionaryFile.getAbsolutePath() + " is not a file.");
            }
            if (!dictionaryFile.canRead())
            {
                throw new FileNotFoundException(dictionaryFile.getAbsolutePath() + " does not have 'read' permissions.");
            }

            return new BufferedReader(new FileReader(dictionaryFile));
        }
        catch (Throwable t)
        {
            return null;
        }
    }

    /**
     * Find the realworld name of the tag given by the key in the hashtable.
     *
     * @param key
     *            the key of the desired name.
     * @return the realworld name (the last enry in
     *         &quot;dicom_dictionary.txt&quot;). if the tag is not in the
     *         hashtable, <code>null</code> is returned.
     */
    public static String getName(FileDicomKey key)
    {
        if (masterHashtable == null)
        {
            parseFile(DEFAULT_DICTIONARY);
        }

        FileDicomTag tag = (FileDicomTag) masterHashtable.get(key);
        if (tag == null)
        {
            return null;
        }
        return tag.getName();
    }

    /**
     * Find the keyword of the tag given by the key in the hashtable.
     *
     * @param key
     *            the key of the desired name.
     * @return the keyword (the 2nd-to-last entry in
     *         &quot;dicom_dictionary.txt&quot;). if the tag is not in the
     *         hashtable, <code>null</code> is returned.
     */
    public static String getKeyword(FileDicomKey key)
    {
        if (masterHashtable == null)
        {
            parseFile(DEFAULT_DICTIONARY);
        }

        FileDicomTag tag = (FileDicomTag) masterHashtable.get(key);
        if (tag == null)
        {
            return null;
        }
        return tag.getKeyword();
    }

    /**
     * Accessor that returns the value multiplicity of the tag. The value
     * multiplicity is how many instances of this value representation (VR)
     * there can be in one tag.
     *
     * @param key
     *            the key of the desired name.
     * @return the value multiplicity if the tag is not in the hashtable,
     *         <code>0</code> is returned.
     */
    public static int getVM(FileDicomKey key)
    {
        if (masterHashtable == null)
        {
            parseFile(DEFAULT_DICTIONARY);
        }

        FileDicomTag tag = (FileDicomTag) masterHashtable.get(key);
        if (tag == null)
        {
            return 0;
        }
        return tag.getVM();
    }

    /**
     * Accessor that returns the value representation of the tag. The value
     * representation allows the reader the read and interpret the tag properly.
     * Because private tags are not unique, the VR is null and they may be read
     * and/or displayed improperly.
     *
     * @param key
     *            the key of the desired name.
     * @return the value representation if the tag is not in the hashtable,
     *         <code>null</code> is returned.
     */
    public static String getVR(FileDicomKey key)
    {
        if (masterHashtable == null)
        {
            parseFile(DEFAULT_DICTIONARY);
        }

        FileDicomTag tag = (FileDicomTag) masterHashtable.get(key);
        if (tag == null)
        {
            return null;
        }
        return tag.getVR();
    }

    public static Hashtable getDicomTagTable()
    {
        return getDicomTagTable(false);
    }

    /**
     * Clones the DICOMHashtable and all its values. returns the clone,
     * presumably to a FileInfoDicom as a template.
     * @return  a deep copy of the dicom tag table
     */
    public static Hashtable getDicomTagTable(boolean forceReload)
    {
        if (masterHashtable == null || forceReload == true)
        {
            parseFile(DEFAULT_DICTIONARY);
        }

        Hashtable clonedHashtable = new Hashtable( (int) (masterHashtable.size() / 0.7));
        Enumeration e = masterHashtable.keys();

        while (e.hasMoreElements())
        {
            Object key = e.nextElement();
            Object value = ( (FileDicomTag) masterHashtable.get(key)).clone();

            clonedHashtable.put(key, value);
        }

        return clonedHashtable;
    }

    public static Hashtable getSubsetDicomTagTable()
    {
        return getSubsetDicomTagTable(false);
    }

    public static Hashtable getSubsetDicomTagTable(boolean forceReload)
    {
        if (subsetHashtable == null || forceReload == true)
        {
            parseFile(SUBSET_DICTIONARY);
        }

        return subsetHashtable;
    }

    /**
     * Find the key of the corresponding tag name in the hashtable. Returns null
     * if it no key is found.
     *
     * @param searchTagName
     *            The name of the tag for which you want the key.
     * @return The key as a String, for example "0010,0028" If the tag name is
     *         not in the hashtable, <code>null</code> is returned.
     */
    public static String getKeyFromTagName(String searchTagName)
    {
        Enumeration enumeration = masterHashtable.keys();

        while (enumeration.hasMoreElements())
        {
            Object key = enumeration.nextElement();

            FileDicomTag tag = (FileDicomTag) masterHashtable.get(key);

            String foundTagName = tag.getName();

            if (foundTagName.equals(searchTagName))
            {
                return ( (FileDicomKey) key).getKey();
            }
        }

        return null;
    }

    /**
     * Sorts the list of tags and returns it as an array order of an ordered
     * list of the keys in the hashtable.
     * <p>
     *
     * @return the sorted list of the tags held.
     */
    private static FileDicomTag[] sortTagsList()
    {
        if (masterHashtable == null)
        {
            parseFile(DEFAULT_DICTIONARY);
        }

        FileDicomTag[] tagsList = new FileDicomTag[masterHashtable.size()];
        FileDicomKey[] hashKeys = sortTagKeys(masterHashtable);
        for (int i = 0; i < tagsList.length; i++)
        {
            tagsList[i] = (FileDicomTag) masterHashtable.get(hashKeys[i]);
        }
        return tagsList;
    }

    /**
     * Sorts the list of tags using a slightly modified shell-sort and returns
     * it as an array in order of FileDicomKeys. This sorting routine is taken
     * from <u>Numerical Recipes in C</u>, 2nd ed. by William H. Press, et al,
     * page 332.
     *
     * @param dicomTagsList
     *            The hashtable of DICOM tags (the keys are FileDicomKey and the
     *            object referred to by that key is the FileDicomTag).
     * @return a sorted array of DICOM Keys.
     */
    public static FileDicomKey[] sortTagKeys(Hashtable dicomTagsList)
    {
        FileDicomKey dicomKeys[];

        dicomKeys = new FileDicomKey[dicomTagsList.size()];
        int q = 0;
        Enumeration e = dicomTagsList.keys();

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

        do
        {
            inc *= 3;
            inc++;
        }
        while (inc <= numDICOMKeys);

        do
        {
            inc /= 3;
            for (int i = inc; i <= numDICOMKeys; i++)
            {
                // orders all from index 0 to numDICOMKeys
                val = dicomKeys[i];
                int j = i;

                /*
                 * use the group and element numbers, gathering them in a way to
                 * accomodate the occaisional general group/element number (like
                 * 60xx).
                 */
                int tempGN, valGN;
                try
                {
                    tempGN = dicomKeys[j - inc].getGroupNumber();
                }
                catch (NumberFormatException nfe)
                {
                    tempGN = Integer.parseInt(dicomKeys[j - inc].getGroup().replace('x', '0'), 0x10);
                }
                try
                {
                    valGN = val.getGroupNumber();
                }
                catch (NumberFormatException nfe)
                {
                    valGN = Integer.parseInt(val.getGroup().replace('x', '0'), 0x10);
                }

                int tempEN, valEN;
                try
                {
                    tempEN = dicomKeys[j - inc].getElementNumber();
                }
                catch (NumberFormatException nfe)
                {
                    tempEN = Integer.parseInt(dicomKeys[j - inc].getElement().replace('x', '0'), 0x10);
                }
                try
                {
                    valEN = val.getElementNumber();
                }
                catch (NumberFormatException nfe)
                {
                    valEN = Integer.parseInt(val.getElement().replace('x', '0'), 0x10);
                }

                // check on both group and element numbers at the same time:
                while (tempGN > valGN || (tempGN == valGN && tempEN > valEN))
                {
                    dicomKeys[j] = dicomKeys[j - inc];
                    j -= inc;
                    if (j <= inc)
                    {
                        break;
                    }

                    // else, reset the temp GN & temp EN
                    try
                    {
                        tempGN = dicomKeys[j - inc].getGroupNumber();
                    }
                    catch (NumberFormatException nfe)
                    {
                        tempGN = Integer.parseInt(dicomKeys[j - inc].getGroup().replace('x', '0'), 0x10);
                    }
                    try
                    {
                        tempEN = dicomKeys[j - inc].getElementNumber();
                    }
                    catch (NumberFormatException nfe)
                    {
                        tempEN = Integer.parseInt(dicomKeys[j - inc].getElement().replace('x', '0'), 0x10);
                    }
                }
                dicomKeys[j] = val;
            }
        }
        while (inc > 1);

        return dicomKeys;
    }

    /**
     * Writes the DICOMHashtable given to the file that the CreateDicomFiles
     * points to.
     *
     * @param dictFile      the dicom dictionary file to write out
     * @param dicomHash     the dicom tag mapping to write out
     *
     * @throws IOException  when the file cannot be written to.
     */
    public static void writeFile(File dictFile, Hashtable dicomHash) throws IOException
    {
        writeFile(dictFile, dicomHash, null);
    }

    /**
     * Writes the DICOMHashtable given to the file that the CreateDicomFiles
     * points to.
     *
     * @param dictFile      the dicom dictionary file to write out
     * @param dicomHash     the dicom tag mapping to write out
     * @param altComment    an additional comment to write out to the file
     *
     * @throws IOException  when the file cannot be written to.
     */
    public static void writeFile(File dictFile, Hashtable dicomHash, String altComment) throws IOException
    {
        if (!dictFile.canWrite())
        {
            throw new IOException(dictFile + " cannot be written.");
        }
        FileWriter fw = new FileWriter(dictFile, false);

        FileDicomKey[] sortedKeys = DICOMDictionaryBuilder.sortTagKeys(dicomHash);

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

        if (altComment != null)
        {
            fw.write(" # " + altComment + "\n");
        }

        for (int i = 0; i < sortedKeys.length; i++)
        {
            FileDicomTag tag = ( (FileDicomTag) dicomHash.get(sortedKeys[i]));
            fw.write("(" + sortedKeys[i].toString() + ")\t");
            fw.write("VERS=\"" + tag.getVersion() + "\"\t");
            fw.write("VR=\"" + tag.getVR() + "\"\t");
            fw.write("VM=\"" + tag.getVM() + "\"\t");
            fw.write("Keyword=\"" + tag.getKeyword() + "\"\t");
            fw.write("Name=\"" + tag.getName() + "\"\n");
        }

        fw.write(" # End of Tag Dictionary file.\n");
        fw.close();
    }

    /**
     * Builds a hashtable of <code>"TYPE_"+FileDicomTag.getVR</code>.
     */
    private static Hashtable hashVREntries()
    {
        if (masterHashtable == null)
        {
            parseFile(DEFAULT_DICTIONARY);
        }

        FileDicomTag tag;

        int i = 1;

        Hashtable hashVR = new Hashtable(masterHashtable.size());
        Enumeration e = masterHashtable.elements();

        hashVR.put(new Integer(0), "TYPE_UNKNOWN");
        while (e.hasMoreElements())
        {
            tag = (FileDicomTag) e.nextElement();
            if (!hashVR.contains("TYPE_" + tag.getVR()))
            {
                hashVR.put(new Integer(i), "TYPE_" + tag.getVR());
                i++;
            }
        }
        return hashVR;
    }

    /**
     *
     */
    private static Hashtable hashKeywordEntries()
    {
        if (masterHashtable == null)
        {
            parseFile(DEFAULT_DICTIONARY);
        }

        FileDicomTag tag;
        int i = 1;

        Hashtable hashKeyword = new Hashtable(masterHashtable.size());
        Enumeration e = masterHashtable.elements();
        while (e.hasMoreElements())
        {
            tag = (FileDicomTag) e.nextElement();
            hashKeyword.put(new Integer(i), "DD_" + tag.getKeyword());
            i++;
        }

        hashKeyword.put(new Integer(i), "DD_LASTSEARCHED");
        hashKeyword.put(new Integer(0), "DD_UNDEFINED");

        return hashKeyword;
    }

    private static Hashtable hashRTCentries(Hashtable kwEntries)
    {
        if (masterHashtable == null)
        {
            parseFile(DEFAULT_DICTIONARY);
        }

        FileDicomTag tag;
        int i = 0;

        Hashtable hashRTCentries = new Hashtable(masterHashtable.size());
        Enumeration e = masterHashtable.elements();
        while (e.hasMoreElements())
        {
            Integer count = new Integer(i);
            tag = (FileDicomTag) e.nextElement();
            hashRTCentries.put(count, "new RTCEntry(0x0" + Integer.toHexString(tag.getGroup()) + ", 0x0"
                               + Integer.toHexString(tag.getElement()) + ", " + "TYPE_" + tag.getVR() + ", " + "\""
                               + tag.getName() + "\"" + ", " +
                               ( (String) kwEntries.get(new Integer(i + 1))).replace('-', '_')
                               + ")");
            i++;
        }

        return hashRTCentries;
    }

    /**
     *
     */
    public static void writeRTCfile(File outputFile) throws IOException, FileNotFoundException
    {
        if (!outputFile.exists())
        {
            try
            {
                outputFile.createNewFile();
            }
            catch (IOException ioe)
            {
                throw ioe;
            }
        }

        if (!outputFile.canWrite())
        {
            throw new FileNotFoundException("Cannot write to " + outputFile.getAbsolutePath());
        }

        FileWriter RTCwriter;
        try
        {
            RTCwriter = new FileWriter(outputFile, false);
            RTCwriter.write(genRTCpackage);
            RTCwriter.write("\n");
            RTCwriter.write(genRTCwarning);
            RTCwriter.write("\n");
            RTCwriter.write(genRTCimports);
            RTCwriter.write("\n");
            RTCwriter.write(genRTCclassRTCEntry);
            RTCwriter.write("\n");
            RTCwriter.write(genRTCjavadocComments);
            RTCwriter.write("");
            RTCwriter.write("public class " + defaultRTCclassName + " { \n");

            Hashtable entryHashVR = hashVREntries();
            int i = 0;
            while (i < entryHashVR.size())
            {
                RTCwriter.write("    public static final int " + entryHashVR.get(new Integer(i)) + " = " + i + ";\n");
                i++;
            }
            RTCwriter.write("\n");

            Hashtable entryHashKeyword = hashKeywordEntries();
            i = 0;
            while (i < entryHashKeyword.size())
            {
                RTCwriter.write("    public static final int "
                                + (entryHashKeyword.get(new Integer(i)).toString().replace('-', '_')) + " = " + i +
                                ";\n");
                i++;
            }
            RTCwriter.write("\n");
            RTCwriter.write(genRTCclassRTCdefineHashtables);
            RTCwriter.write("\n");
            RTCwriter.write(genRTCclassRTCarrayDef);

            Hashtable classEntryHash = hashRTCentries(entryHashKeyword);
            i = 1;

            while (i < classEntryHash.size() - 1)
            {
                RTCwriter.write("        " + classEntryHash.get(new Integer(i)) + ",");
                RTCwriter.write("\n");
                i++;
            }
            RTCwriter.write("        " + classEntryHash.get(new Integer(i)) + "\n");
            RTCwriter.write("    };  // End RTCEntry[] \n");
            RTCwriter.write("\n");

            RTCwriter.write(genRTCclassRTCtypecodeComments);
            RTCwriter.write("    public static String getTypeCodeName" + "(int dd_type) { \n");
            RTCwriter.write("        String s = null;\n");
            RTCwriter.write("        int i = getIndex(dd_type); \n");
            RTCwriter.write("\n");
            RTCwriter.write("        if (i >= 0) { \n");
            RTCwriter.write("            if (!typeCodeNamesFilled) { \n");
            i = 1;
            while (i < entryHashVR.size())
            {
                Integer count = new Integer(i);
                String typeString = (String) entryHashVR.get(count);
                String vrString = "\"" + ( (String) entryHashVR.get(count)).substring(5) + "\"";

                RTCwriter.write("                " + "typeCodeNames.put(new Integer(" + typeString + "), " + vrString
                                + "); \n");
                i++;
            }
            RTCwriter.write("                typeCodeNamesFilled = true; \n");
            RTCwriter.write("            } \n");
            RTCwriter.write(genRTCclassRTCsetRTCcodeName);
            RTCwriter.write("        } \n");
            RTCwriter.write(genRTCclassRTCsetUnkownRTCcodeName);
            RTCwriter.write("        return s;\n    } \n");

            RTCwriter.write(genRTCclassRTCaddTypeHashComments);
            RTCwriter.write(genRTCclassRTCaddTypeHashMethod);
            RTCwriter.write("\n");
            RTCwriter.write(genRTCclassRTCstaticSearchGroup);
            RTCwriter.write("\n");
            RTCwriter.write(genRTCclassRTCgetIndexComments);
            RTCwriter.write(genRTCclassRTCgetIndexMethod);
            RTCwriter.write("\n");
            RTCwriter.write(genRTCclassRTCunknownDDTypeComments);
            RTCwriter.write(genRTCclassRTCunknownDDTypeMethod);
            RTCwriter.write("\n");
            RTCwriter.write(genRTCclassRTCgetGroupComments);
            RTCwriter.write(genRTCclassRTCgetGroupMethod);
            RTCwriter.write("\n");
            RTCwriter.write(genRTCclassRTCgetElementComments);
            RTCwriter.write(genRTCclassRTCgetElementMethod);
            RTCwriter.write("\n");
            RTCwriter.write(genRTCclassRTCgetTypeCodeComments);
            RTCwriter.write(genRTCclassRTCgetTypeCodeMethod);
            RTCwriter.write("\n");
            RTCwriter.write(genRTCclassRTCgetDescriptionComments);
            RTCwriter.write(genRTCclassRTCgetDescriptionMethod);

            RTCwriter.write("}  // end RTC\n");

            // write to disk
            RTCwriter.flush();
            RTCwriter.close();
        }
        catch (IOException ioe)
        {
            throw ioe;
        }

    }

    final static private String genRTCwarning = " // ***********************************************************\n"
        + " // WARNING: This file is generated automatically. \n"
        + " // Any edits will be lost upon regeneration. \n" + " // Please modify the generator instead. \n"
        + " // See CreateDICOMFiles \n" + " // ***********************************************************\n"
        + "\n" + " // please see the copyright notice in the LICENSE.txt file\n";

    final static private String genRTCpackage = "package gov.nih.mipav.model.dicomcomm;\n" + "";

    final static private String genRTCimports = "import java.util.Hashtable;\n" + "";

    final static private String genRTCclassRTCEntry = "\n" + "/** \n" + " * \n" + " */ \n" + "class RTCEntry {\n"
        + "    int    group; \n" + "    int    element; \n" + "    int    typeCode; \n"
        + "    String description; \n" + "    int    dd_type; \n" + "\n"
        + "    /** Loads the inputted variabels straight into the class.\n" + "     * \n" + "     */ \n"
        + "    RTCEntry(int group, int element, \n"
        + "             int typeCode, String description, int dd_type ) \n" + "    { \n"
        + "        this.group       = group; \n" + "        this.element     = element; \n"
        + "        this.typeCode    = typeCode; \n" + "        this.description = description; \n"
        + "        this.dd_type     = dd_type; \n" + "    } \n" + "} \n";

    final static private String genRTCjavadocComments = "\n" + "/** \n"
        + " * RTC - Run Time Class (although it is actually statically " + "defined here)  \n"
        + " * This allows access to information in the DICOM data dictionary.\n"
        + " * Each group, element pair is accessed via a unique static \n"
        + " * integer variable defined here.  This allows for extremely \n"
        + " * fast access to this table in a readable source code fashion. \n"
        + " * All integer hash indices are easily readable representations of \n"
        + " * group-element pairs.  For example, (0010,0010) is represented \n"
        + " * by RTC.DD_PatientName.  (0010,0020) is RTC.DD_PatientID, etc.  \n"
        + " * If there is a group-element pair you require use of which is \n"
        + " * not hashed as an integer here, use the <A HREF="
        + "\"#unknownDDType\">\n * unknownDDType()</A> method to hash it \n"
        + " * on the fly and receive a new integer hash index." + " * <p>\n"
        + " * Once an integer hash index is available, the following \n" + " * methods may be called: \n"
        + " * <A HREF=\"#getGroup\">getGroup()</A>, \n" + " * <A HREF=\"#getElement\">getElement()</A>, \n"
        + " * <A HREF=\"#getTypeCode\">getTypeCode()</A>, and \n"
        + " * <A HREF=\"#getTypeCodeName\">getTypeCodeName()</A>, and \n"
        + " * <A HREF=\"#getDescription\">getDescription()</A>\n" + " * <p>\n"
        + " * See the main() routine in the source for usage example \n" + " */  \n";

    final static private String genRTCclassRTCName = "public class DICOM_RTC { \n";

    final static private String genRTCclassRTCtypedef0 = "    public static final int TYPE_UNKNOWN = 0;\n";

    final static private String genRTCclassRTCtypedefGeneral = "    public static final int TYPE_";

    final static private String genRTCclassRTCattributeDefComments =
        "    // easy by-name access to all data dictionary entries."
        + "    // these are hashed for quick inquiries....";

    final static private String genRTCclassRTCdddef0 = "    public static final int DD_UNDEFINED = 0;";

    final static private String genRTCclassRTCdefineHashtables =
        "    private static Hashtable ddTypeIndexes = new Hashtable();\n"
        + "    private static Hashtable typeCodeNames = new Hashtable();\n"
        + "    private static boolean typeCodeNamesFilled = false;\n";

    final static private String genRTCclassRTCarrayDef = "    // array of RTC Entries:\n"
        + "    private static RTCEntry[] rtcList = {\n";

    final static private String genRTCclassRTCtypecodeComments = "    /** \n"
        + "     * gets the type code name of a given integer hash index \n"
        + "     * for example: getTypeCodeName( RTC.DD_PatientID ) " + "returns \"LO\" \n"
        + "     * @param  dd_type the integer hash index to use \n"
        + "     * (see huge list of DD_XXX constants) \n" + "     * @return         the type code name string \n"
        + "     */ \n";

    final static private String genRTCclassRTCsetRTCcodeName = "            "
        + "s = (String) typeCodeNames.get(new Integer(rtcList[i].typeCode )); \n";

    final static private String genRTCclassRTCsetUnkownRTCcodeName = "        if (s == null) { \n"
        + "            s = \"??\";\n" + "        } \n";

    final static private String genRTCclassRTCaddTypeHashComments = "   /** \n" + "    * \n" + "    */ \n";

    final static private String genRTCclassRTCaddTypeHashMethod =
        "    private static void addDDTypeHash( int g, int e, int dd_type ) \n"
        + "    { \n"
        + "        if( unknownDDType( g, e ) == DD_LASTSEARCHED ) { \n"
        + "            ddTypeIndexes.put( new Integer( dd_type ), "
        + "new Integer( lastSearchIndex ) ); "
        + "        } \n" + "    } \n";

    final static private String genRTCclassRTCstaticSearchGroup = "    static int      lastSearchGroup        = 0; \n"
        + "    static int      lastSearchElement      = 0; \n"
        + "    static int      lastSearchIndex        = 0;\n";

    final static private String genRTCclassRTCgetIndexComments = "   /** \n" + "    * \n" + "    */ \n";

    final static private String genRTCclassRTCgetIndexMethod = "    private static int getIndex( int dd_type ) \n"
        + "    { \n" + "        Integer index; \n" + "\n" + "        // did the caller just do a search? \n"
        + "        if( dd_type == DD_LASTSEARCHED ) { \n" + "            return( lastSearchIndex ); \n"
        + "        } \n" +

        "        // shouldn't be this... \n" + "        if( dd_type == DD_UNDEFINED ) { \n"
        + "            return( -1 ); \n" + "        } \n" +

        "        // is it in hash? \n"
        + "        index = (Integer) ddTypeIndexes.get( new Integer( dd_type ) ); \n"
        + "        if( index != null ) {\n" + "            return( index.intValue() ); \n" + "        } \n" + "\n"
        + "        // no? well, then put it there... \n" + "        for( int i=0; i<rtcList.length; i++ ) { \n"
        + "            if( rtcList[i].dd_type == dd_type ) { \n"
        + "                addDDTypeHash( rtcList[i].group, rtcList[i].element, dd_type ); \n"
        + "                return( i ); \n" + "            } \n" + "        } \n" +

        "        // user gave us an unknown dd_type ?!? \n"
        + "        throw new RuntimeException( \"RTC data dictionary hash failure "
        + "        (passed type \" + dd_type + \")\" ); \n" + "    } \n";

    final static private String genRTCclassRTCunknownDDTypeComments = "    /** \n"
        + "    * hash a group, element pair that is not already present in our \n" + "    * list. \n"
        + "    * @return a dd_type integer hash index for use with the other methods here. \n" + "    */ \n";

    final static private String genRTCclassRTCunknownDDTypeMethod =
        "    public static int unknownDDType( int g, int e ) \n"
        + "    { \n"
        + "        if( e == 0x0000 ) { \n"
        + "            return( DD_GroupLength ); // handled special \n"
        + "        } \n"
        + "        if( g == lastSearchGroup && e == lastSearchElement ) { \n"
        + "            // we just looked up this one... no need to repeat... \n"
        + "            return( DD_LASTSEARCHED ); \n"
        + "        } \n"
        + "        else { \n"
        + "            // do an exhaustive search... \n"
        + "            for( int i=0; i<rtcList.length; i++ ) { \n"
        + "                if( g == rtcList[i].group ) { \n"
        + "                    if( e == rtcList[i].element ) { \n"
        + "                        lastSearchGroup   = g; \n"
        + "                        lastSearchElement = e; \n"
        + "                        lastSearchIndex   = i; \n"
        + "                        int dd_type       = rtcList[i].dd_type; \n"
        + " \n"
        + "                        if( dd_type == DD_UNDEFINED ) { \n"
        + "                            dd_type = DD_LASTSEARCHED; \n"
        + "                        } \n"
        + "                        return( dd_type ); \n"
        + "                    } \n"
        + "                } \n"
        + "            } \n" + "        } \n" + " \n" + "        return( DD_UNDEFINED ); \n" + "    } \n";

    final static private String genRTCclassRTCgetGroupComments = "    /** \n"
        + "    * gets the group number of a given integer hash index \n" + "    * \n"
        + "    * for example: getGroup( RTC.DD_PatientID ) returns 0x0010 \n" + "    * \n"
        + "    * @param  dd_type the integer hash index to use (see huge list of DD_XXX constants) \n"
        + "    * @return         the group number \n" + "    */ \n";

    final static private String genRTCclassRTCgetGroupMethod = "    public static int getGroup( int dd_type ) \n"
        + "    { \n" + "        int i; \n" + " \n" + "        return( \n"
        + "            ( (i=getIndex(dd_type)) >= 0 ) ?  \n" + "                rtcList[i].group \n"
        + "            : \n" + "                -1 // shouldn't get here \n" + "            ); \n" + "    } \n";

    final static private String genRTCclassRTCgetElementComments = "    /** \n"
        + "    * gets the element number of a given integer hash index \n" + "    * \n"
        + "    * for example: getElement( RTC.DD_PatientID ) returns 0x0020 \n" + "    * \n"
        + "    * @param  dd_type the integer hash index to use (see huge list of DD_XXX constants) \n"
        + "    * @return         the element number \n" + "    */ \n";

    final static private String genRTCclassRTCgetElementMethod = "    public static int getElement( int dd_type ) \n"
        + "    { \n" + "        int i; \n" + " \n" + "        return( \n"
        + "            ( (i=getIndex(dd_type)) >= 0 ) ?  \n" + "                rtcList[i].element \n"
        + "            : \n" + "                -1 // shouldn't get here \n" + "            ); \n" + "    } \n";

    final static private String genRTCclassRTCgetTypeCodeComments = "   /** \n"
        + "   * gets the type code of a given integer hash index \n" + "   * \n"
        + "   * for example: getTypeCode( RTC.DD_PatientID ) returns RTC.TYPE_LO \n" + "   * \n"
        + "   * @param  dd_type the integer hash index to use (see huge list of DD_XXX constants) \n"
        + "   * @return         the type code (see list of TYPE_XXX constants) \n" + "   */ \n";

    final static private String genRTCclassRTCgetTypeCodeMethod = "    public static int getTypeCode( int dd_type ) \n"
        + "    { \n" + "        int i; \n" + " \n" + "        return( \n"
        + "            ( (i=getIndex(dd_type)) >= 0 ) ?  \n" + "                rtcList[i].typeCode \n"
        + "            : \n" + "                TYPE_UNKNOWN \n" + "            ); \n" + "    } \n";

    final static private String genRTCclassRTCgetDescriptionComments = "    /** \n"
        + "    * gets the description of a given integer hash index \n" + "    * \n"
        + "    * for example: getElement( RTC.DD_PatientID ) returns \"Patient's ID\" \n" + "    * \n"
        + "    * @param  dd_type the integer hash index to use (see huge list of DD_XXX constants) \n"
        + "    * @return         the description string \n" + "    */ \n";

    final static private String genRTCclassRTCgetDescriptionMethod =
        "    public static String getDescription( int dd_type ) \n"
        + "    { \n"
        + "        int i; \n"
        + " \n"
        + "        return( \n"
        + "            ( (i=getIndex(dd_type)) >= 0 ) ?  \n"
        + "                rtcList[i].description \n"
        + "            : \n" + "                \"Unknown\" \n" + "            ); \n" + "    } \n";

    final static private String createDICOMFilesCommandLineHelp = "\n" + "Call with:\njava CreateDICOMFiles "
        + "[-h|-help] [file [path [file [path]]]] \n" + "options: \n "
        + "\t-h, -help:   generate this help then exit \n" + "\tfirst file:  DICOM dictionary file \n"
        + "\tfirst path:  path to DICOM dictionary \n" + "\tsecond file: generated source file name \n"
        + "\tsecond path: generated source file output directory \n"
        + "\nNo arguments will generate a source file " + "\"RTC_DICOM.java\" from \"elmdict.tpl\" \n"
        + "both in the user home directory. \n" + "\nTake care when specifying file paths, that quotes do not get "
        + "'commented out' \nby placing a backslash next to a double-quote.";

}
