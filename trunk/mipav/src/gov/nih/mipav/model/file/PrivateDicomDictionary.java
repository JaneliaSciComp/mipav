package gov.nih.mipav.model.file;

import gov.nih.mipav.model.file.FileDicomTagInfo.VR;
import gov.nih.mipav.view.GetPath;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.GetPath.Purpose;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.StringTokenizer;
import java.util.TreeMap;

/**
 * Note does not use hashtable since two Private keys can have same hashcode.
 * 
 * @author justinsenseney
 *
 */
public class PrivateDicomDictionary {
	
	/** default dictionary file name, &quot;dicom_dictionary.txt&quot;. */
    public static final String DEFAULT_DICTIONARY_FILENAME = "private_dictionary.txt";
	
    /** subset dictionary file name, &quot;dicomsave.dictionary&quot;. */
    public static final String SUBSET_DICTIONARY_FILENAME = "privatedicomsave.dictionary";
    
	private static final String PUBLISHER = "Publisher:";
	
	/** DOCUMENT ME! */
    protected static final int DEFAULT_DICTIONARY = 1;

    /** DOCUMENT ME! */
    protected static final int SUBSET_DICTIONARY = 2;
	
	/** TreeMap filled with known DICOM tags with empty value attributes. */
    private static TreeMap<PrivateFileDicomKey, FileDicomTagInfo> masterPrivateTreeMap;
    
    /**
     * TreeMap filled with DICOM tags which are a subset (not necessarily a proper subset) of dicom tags in the master
     * table. This subset is then used to export dicom tags to the XML image format.
     */
    private static TreeMap<PrivateFileDicomKey, FileDicomTagInfo> subsetTreeMap;
	
	public static boolean privateDictionaryProcessed = false;
	
	/**
     * Returns a reference to the DICOM Hashtable.
     * 
     * @param forceReload If true, forces the master tag table to be re-read from the dicom dictionary file
     * 
     * @return a reference to the dicom tag table
     */
    public static TreeMap<PrivateFileDicomKey, FileDicomTagInfo> getPrivateDicomTagTable(final boolean forceReload) {

        if ( (doParseFile()) || (forceReload == true)) {
            parseFile(DEFAULT_DICTIONARY);
        }

        final TreeMap<PrivateFileDicomKey, FileDicomTagInfo> clonedHashtable = new TreeMap<PrivateFileDicomKey, FileDicomTagInfo>();
        final Iterator<PrivateFileDicomKey> e = masterPrivateTreeMap.keySet().iterator();

        while (e.hasNext()) {
            final PrivateFileDicomKey key = e.next();
            final FileDicomTagInfo value = (FileDicomTagInfo) masterPrivateTreeMap.get(key).clone();

            clonedHashtable.put(key, value);
        }

        return clonedHashtable;
    }
    
    /**
     * Returns whether the dicom dictionary contains a tag with the given key identifier.
     * 
     * @param key the key for this tag
     * 
     * @return whether a tag matching the given key is contained in the dicom dictionary.
     */
    public static boolean containsTag(PrivateFileDicomKey key) {

        if (doParseFile()) {
            parseFile(DEFAULT_DICTIONARY);
        }
        boolean found = false;
        
        if(!(found = masterPrivateTreeMap.containsKey(key))) {
        	PrivateFileDicomKey subKey = new PrivateFileDicomKey(key.getPublisher(), key.getGroup()+",xx"+key.getElement().substring(2, 4));
        	found = masterPrivateTreeMap.containsKey(subKey);
        }

        return found;
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
                filepath = GetPath.getPath(filename, Purpose.FOR_READING) + File.separator;
            }

            if (filepath == null || filepath.equals(File.separator)) {
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
    
    private static final FileDicomTagInfo groupLengthInfo = new FileDicomTagInfo(null, VR.SH, 1, "Group Length", "Group Length");
    private static final FileDicomTagInfo groupNameInfo = new FileDicomTagInfo(null, VR.SH, 1, "Group Name", "Group Name");
    
    /**
     * Return information about a key in the dicom dictionary.
     * 
     * @param key the key to retreive information about
     * 
     * @return information about the requested key
     */
    public static FileDicomTagInfo getInfo(PrivateFileDicomKey key) {

        if (doParseFile()) {
            parseFile(DEFAULT_DICTIONARY);
        }

    	FileDicomTagInfo info = null;
    	if(key.getElement().equals("0000")) {
    		info = (FileDicomTagInfo) groupLengthInfo.clone();
    	} else if(key.getElement().equals("0010")) {
    		info = (FileDicomTagInfo) groupNameInfo.clone();
    	} else {
        	FileDicomKey subKey = convertToWildKey((PrivateFileDicomKey)key);
        	info = masterPrivateTreeMap.get(subKey);
    	}
    	if(info != null) {
    		info.setKey(key);
    	}
    	return info;
    }
    
    /**
     * Converts group numbers of 60xx or 50xx dicom key elements so that the dicom dictionary will be able to find them.
     */

    protected static FileDicomKey convertToWildKey(final PrivateFileDicomKey key) {
    	PrivateFileDicomKey subKey = new PrivateFileDicomKey(key.getPublisher(), key.getGroup()+",xx"+key.getElement().substring(2, 4));
    	
    	return subKey;
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
        final TreeMap<PrivateFileDicomKey, FileDicomTagInfo> treeMap = new TreeMap<PrivateFileDicomKey, FileDicomTagInfo>();

        if (dictionary_type == SUBSET_DICTIONARY) {
            filename = SUBSET_DICTIONARY_FILENAME;
        } else {
            filename = DEFAULT_DICTIONARY_FILENAME;
        }

        BufferedReader dictionaryReference = getFileReader(filename);

        if (dictionaryReference == null) {
            Preferences.debug("Failed to read DICOM dictionary file from " + filename, Preferences.DEBUG_FILEIO);
            Preferences.debug("If saving a dicom image, no tags will be saved.", Preferences.DEBUG_FILEIO);
            return;
        }
        
        String publisher = PrivateFileDicomKey.NO_PUBLISHER;

        PrivateFileDicomKey key = null;

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
                
                if(s.contains(PUBLISHER)) {
                	publisher = s.substring(s.indexOf(PUBLISHER) + PUBLISHER.length()).trim();
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
                key = new PrivateFileDicomKey(publisher, values.substring(1, 10));

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

                    FileDicomTagInfo tagInfo = treeMap.put(key, new FileDicomTagInfo(key, vers, vr, vm, keyword, name));
                    if(tagInfo != null) {
                    	System.err.println("Key already defined in private dicom dictionary: "+key);
                    }
                } else {

                    if (values.lastIndexOf("Keyword") == -1) {
                        values = (String) tok.nextElement();
                        values = (String) tok.nextElement();
                        index = values.lastIndexOf("\"");

                        final String keyword = values.substring(1, index);

                        values = (String) tok.nextElement();
                        index = values.lastIndexOf("\"");

                        final String name = values.substring(1, index);
                        FileDicomTagInfo tagInfo = treeMap.put(key, new FileDicomTagInfo(key, vers, vr, vm, keyword, name));
                        if(tagInfo != null) {
                        	System.err.println("Key already defined in private dicom dictionary: "+key);
                        }
                    } else {
                        values = (String) tok.nextElement();
                        index = values.lastIndexOf("\"");

                        final String keyword = values.substring(1, index);

                        values = (String) tok.nextElement();
                        index = values.lastIndexOf("\"");

                        final String name = values.substring(1, index);
                        if(treeMap.containsKey(key)) {
                        	FileDicomTagInfo tagInfo = treeMap.get(key);
                        	System.out.println("False");
                        }
                        FileDicomTagInfo tagInfo = treeMap.put(key, new FileDicomTagInfo(key, vers, vr, vm, keyword, name));
                       
                        if(tagInfo != null) {
                        	System.err.println("Key already defined in private dicom dictionary: "+key);
                        }
                    }
                }
            }
        } catch (final FileNotFoundException fnfe) {
            Preferences.debug("Dictionary file not found: " + filename, Preferences.DEBUG_FILEIO);
            MipavUtil.displayWarning("Dictionary file not found: " + filename);
        } catch (final Exception e) {
            MipavUtil.displayError("Failed to parse DICOM dictionary file at tag: "+key);
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

            if (dictionary_type == SUBSET_DICTIONARY) {
                subsetTreeMap = treeMap;
            } else {
            	masterPrivateTreeMap = treeMap;
            }
        }
    }
    
    protected static boolean doParseFile() {
    	if(!privateDictionaryProcessed || masterPrivateTreeMap == null) {
    		privateDictionaryProcessed = true;
    		return true;
    	} else {
    		return false;
    	}
    }

}
