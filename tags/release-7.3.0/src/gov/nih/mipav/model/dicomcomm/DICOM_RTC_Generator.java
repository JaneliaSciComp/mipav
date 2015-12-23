package gov.nih.mipav.model.dicomcomm;


import gov.nih.mipav.model.file.*;

import java.io.*;
import java.util.*;


/**
 * This class generates the DICOM_RTC class file from the master DICOM tag list file (whose entries are obtained through
 * calls to DicomDictionary). The methods in this class are used infrequently to re-generate the DICOM_RTC class, such
 * as when the default dicom dictionary entries are changed.
 * 
 * <hr>
 * 
 * This DICOM communication package was originally based on the Java Dicom Package, whose license is below:
 * 
 * <pre>
 * Java Dicom Package (com.zmed.dicom)
 * 
 *  Copyright (c) 1996-1997 Z Medical Imaging Systems, Inc.
 * 
 *  This software is provided, as is, for non-commercial educational
 *  purposes only.   Use or incorporation of this software or derivative
 *  works in commercial applications requires written consent from
 *  Z Medical Imaging Systems, Inc.
 * 
 *  Z MEDICAL IMAGING SYSTEMS MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT
 *  THE SUITABILITY OF THE SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING
 *  BUT NOT LIMITED TO THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS
 *  FOR A PARTICULAR PURPOSE, NON-INFRINGEMENT, OR CONFORMANCE TO ANY
 *  SPECIFICATION OR STANDARD.  Z MEDICAL IMAGING SYSTEMS SHALL NOT BE
 *  LIABLE FOR ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF USING OR
 *  MODIFYING THIS SOFTWARE OR ITS DERIVATIVES.
 * 
 *  =============================================================================
 * 
 *  This software package is implemented similarly to the UC Davis public
 *  domain C++ DICOM implementation which contains the following copyright
 *  notice:
 * 
 *  Copyright (C) 1995, University of California, Davis
 * 
 *  THIS SOFTWARE IS MADE AVAILABLE, AS IS, AND THE UNIVERSITY
 *  OF CALIFORNIA DOES NOT MAKE ANY WARRANTY ABOUT THE SOFTWARE, ITS
 *  PERFORMANCE, ITS MERCHANTABILITY OR FITNESS FOR ANY PARTICULAR
 *  USE, FREEDOM FROM ANY COMPUTER DISEASES OR ITS CONFORMITY TO ANY
 *  SPECIFICATION. THE ENTIRE RISK AS TO QUALITY AND PERFORMANCE OF
 *  THE SOFTWARE IS WITH THE USER.
 * 
 *  Copyright of the software and supporting documentation is
 *  owned by the University of California, and free access
 *  is hereby granted as a license to use this software, copy this
 *  software and prepare derivative works based upon this software.
 *  However, any distribution of this software source code or
 *  supporting documentation or derivative works (source code and
 *  supporting documentation) must include this copyright notice.
 * 
 *  The UC Davis C++ source code is publicly available from the following
 *  anonymous ftp site:
 * 
 *  ftp://imrad.ucdmc.ucdavis.edu/pub/dicom/UCDMC/
 * </pre>
 * 
 * @see DicomDictionary
 * @see DICOM_RTC
 */
public class DICOM_RTC_Generator {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /**
     * Default classname for the generated file being used in the Run Time Class, which allows access to information in
     * the DICOM data dictionary. It is created for use with the dicomcomm package, a UC-Berkeley-originated collection
     * objects.
     */
    protected static final String defaultRTCclassName = "DICOM_RTC";

    /** Default output file name for the RTC source output, &quot;DICOM_RTC.java&quot;. */
    public static String default_RTC_name = DICOM_RTC_Generator.defaultRTCclassName + ".java";

    /** DOCUMENT ME! */
    private static final String genRTCwarning = ""
            + " // ***********************************************************\n"
            + " // WARNING: This file is generated automatically. \n"
            + " // Any edits will be lost upon regeneration. \n" + " // Please modify the generator instead. \n"
            + " // See DICOM_RTC_Generator \n" + " // ***********************************************************\n"
            + "\n" + " // please see the copyright notice in the javadocs for the dicomcomm package classes\n";

    /** DOCUMENT ME! */
    private static final String genRTCpackage = "package gov.nih.mipav.model.dicomcomm;\n" + "";

    /** DOCUMENT ME! */
    private static final String genRTCimports = "import java.util.Hashtable;\n" + "";

    /** DOCUMENT ME! */
    private static final String genRTCclassRTCEntry = "\n" + "/** \n" + " * \n" + " */ \n" + "class RTCEntry {\n"
            + "    int    group; \n" + "    int    element; \n" + "    int    typeCode; \n"
            + "    String description; \n" + "    int    dd_type; \n" + "\n"
            + "    /** Loads the inputted variabels straight into the class.\n" + "     * \n" + "     */ \n"
            + "    RTCEntry(int group, int element, \n"
            + "             int typeCode, String description, int dd_type ) \n" + "    { \n"
            + "        this.group       = group; \n" + "        this.element     = element; \n"
            + "        this.typeCode    = typeCode; \n" + "        this.description = description; \n"
            + "        this.dd_type     = dd_type; \n" + "    } \n" + "} \n";

    /** DOCUMENT ME! */
    private static final String genRTCjavadocComments = "\n" + "/** \n"
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

    /** DOCUMENT ME! */
    private static final String genRTCclassRTCName = "public class DICOM_RTC { \n";

    /** DOCUMENT ME! */
    private static final String genRTCclassRTCtypedef0 = "    public static final int TYPE_UNKNOWN = 0;\n";

    /** DOCUMENT ME! */
    private static final String genRTCclassRTCtypedefGeneral = "    public static final int TYPE_";

    /** DOCUMENT ME! */
    private static final String genRTCclassRTCattributeDefComments = "    // easy by-name access to all data dictionary entries."
            + "    // these are hashed for quick inquiries....";

    /** DOCUMENT ME! */
    private static final String genRTCclassRTCdddef0 = "    public static final int DD_UNDEFINED = 0;";

    /** DOCUMENT ME! */
    private static final String genRTCclassRTCdefineHashtables = "    private static Hashtable ddTypeIndexes = new Hashtable();\n"
            + "    private static Hashtable typeCodeNames = new Hashtable();\n"
            + "    private static boolean typeCodeNamesFilled = false;\n";

    /** DOCUMENT ME! */
    private static final String genRTCclassRTCarrayDef = "    // array of RTC Entries:\n"
            + "    private static RTCEntry[] rtcList = {\n";

    /** DOCUMENT ME! */
    private static final String genRTCclassRTCtypecodeComments = "    /** \n"
            + "     * gets the type code name of a given integer hash index \n"
            + "     * for example: getTypeCodeName( RTC.DD_PatientID ) " + "returns \"LO\" \n"
            + "     * @param  dd_type the integer hash index to use \n"
            + "     * (see huge list of DD_XXX constants) \n" + "     * @return         the type code name string \n"
            + "     */ \n";

    /** DOCUMENT ME! */
    private static final String genRTCclassRTCsetRTCcodeName = "            "
            + "s = (String) typeCodeNames.get(new Integer(rtcList[i].typeCode )); \n";

    /** DOCUMENT ME! */
    private static final String genRTCclassRTCsetUnkownRTCcodeName = "        if (s == null) { \n"
            + "            s = \"??\";\n" + "        } \n";

    /** DOCUMENT ME! */
    private static final String genRTCclassRTCaddTypeHashComments = "   /** \n" + "    * \n" + "    */ \n";

    /** DOCUMENT ME! */
    private static final String genRTCclassRTCaddTypeHashMethod = "    private static void addDDTypeHash( int g, int e, int dd_type ) \n"
            + "    { \n"
            + "        if( unknownDDType( g, e ) == DD_LASTSEARCHED ) { \n"
            + "            ddTypeIndexes.put( new Integer( dd_type ), "
            + "new Integer( lastSearchIndex ) ); "
            + "        } \n" + "    } \n";

    /** DOCUMENT ME! */
    private static final String genRTCclassRTCstaticSearchGroup = "    static int      lastSearchGroup        = 0; \n"
            + "    static int      lastSearchElement      = 0; \n"
            + "    static int      lastSearchIndex        = 0;\n";

    /** DOCUMENT ME! */
    private static final String genRTCclassRTCgetIndexComments = "   /** \n" + "    * \n" + "    */ \n";

    /** DOCUMENT ME! */
    private static final String genRTCclassRTCgetIndexMethod = "    private static int getIndex( int dd_type ) \n"
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

    /** DOCUMENT ME! */
    private static final String genRTCclassRTCunknownDDTypeComments = "    /** \n"
            + "    * hash a group, element pair that is not already present in our \n" + "    * list. \n"
            + "    * @return a dd_type integer hash index for use with the other methods here. \n" + "    */ \n";

    /** DOCUMENT ME! */
    private static final String genRTCclassRTCunknownDDTypeMethod = "    public static int unknownDDType( int g, int e ) \n"
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

    /** DOCUMENT ME! */
    private static final String genRTCclassRTCgetGroupComments = "    /** \n"
            + "    * gets the group number of a given integer hash index \n" + "    * \n"
            + "    * for example: getGroup( RTC.DD_PatientID ) returns 0x0010 \n" + "    * \n"
            + "    * @param  dd_type the integer hash index to use (see huge list of DD_XXX constants) \n"
            + "    * @return         the group number \n" + "    */ \n";

    /** DOCUMENT ME! */
    private static final String genRTCclassRTCgetGroupMethod = "    public static int getGroup( int dd_type ) \n"
            + "    { \n" + "        int i; \n" + " \n" + "        return( \n"
            + "            ( (i=getIndex(dd_type)) >= 0 ) ?  \n" + "                rtcList[i].group \n"
            + "            : \n" + "                -1 // shouldn't get here \n" + "            ); \n" + "    } \n";

    /** DOCUMENT ME! */
    private static final String genRTCclassRTCgetElementComments = "    /** \n"
            + "    * gets the element number of a given integer hash index \n" + "    * \n"
            + "    * for example: getElement( RTC.DD_PatientID ) returns 0x0020 \n" + "    * \n"
            + "    * @param  dd_type the integer hash index to use (see huge list of DD_XXX constants) \n"
            + "    * @return         the element number \n" + "    */ \n";

    /** DOCUMENT ME! */
    private static final String genRTCclassRTCgetElementMethod = "    public static int getElement( int dd_type ) \n"
            + "    { \n" + "        int i; \n" + " \n" + "        return( \n"
            + "            ( (i=getIndex(dd_type)) >= 0 ) ?  \n" + "                rtcList[i].element \n"
            + "            : \n" + "                -1 // shouldn't get here \n" + "            ); \n" + "    } \n";

    /** DOCUMENT ME! */
    private static final String genRTCclassRTCgetTypeCodeComments = "   /** \n"
            + "   * gets the type code of a given integer hash index \n" + "   * \n"
            + "   * for example: getTypeCode( RTC.DD_PatientID ) returns RTC.TYPE_LO \n" + "   * \n"
            + "   * @param  dd_type the integer hash index to use (see huge list of DD_XXX constants) \n"
            + "   * @return         the type code (see list of TYPE_XXX constants) \n" + "   */ \n";

    /** DOCUMENT ME! */
    private static final String genRTCclassRTCgetTypeCodeMethod = "    public static int getTypeCode( int dd_type ) \n"
            + "    { \n" + "        int i; \n" + " \n" + "        return( \n"
            + "            ( (i=getIndex(dd_type)) >= 0 ) ?  \n" + "                rtcList[i].typeCode \n"
            + "            : \n" + "                TYPE_UNKNOWN \n" + "            ); \n" + "    } \n";

    /** DOCUMENT ME! */
    private static final String genRTCclassRTCgetDescriptionComments = "    /** \n"
            + "    * gets the description of a given integer hash index \n" + "    * \n"
            + "    * for example: getElement( RTC.DD_PatientID ) returns \"Patient's ID\" \n" + "    * \n"
            + "    * @param  dd_type the integer hash index to use (see huge list of DD_XXX constants) \n"
            + "    * @return         the description string \n" + "    */ \n";

    /** DOCUMENT ME! */
    private static final String genRTCclassRTCgetDescriptionMethod = "    public static String getDescription( int dd_type ) \n"
            + "    { \n"
            + "        int i; \n"
            + " \n"
            + "        return( \n"
            + "            ( (i=getIndex(dd_type)) >= 0 ) ?  \n"
            + "                rtcList[i].description \n"
            + "            : \n" + "                \"Unknown\" \n" + "            ); \n" + "    } \n";

    /** DOCUMENT ME! */
    private static final String createDICOMFilesCommandLineHelp = "\n" + "Call with:\njava DICOM_RTC_Generator "
            + "[-h|-help] [file [path [file [path]]]] \n" + "options: \n "
            + "\t-h, -help:   generate this help then exit \n" + "\tfirst file:  DICOM dictionary file \n"
            + "\tfirst path:  path to DICOM dictionary \n" + "\tsecond file: generated source file name \n"
            + "\tsecond path: generated source file output directory \n"
            + "\nNo arguments will generate a source file " + "\"RTC_DICOM.java\" from \"elmdict.tpl\" \n"
            + "both in the user home directory. \n" + "\nTake care when specifying file paths, that quotes do not get "
            + "'commented out' \nby placing a backslash next to a double-quote.";

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     * 
     * @param outputFile DOCUMENT ME!
     * 
     * @throws IOException DOCUMENT ME!
     * @throws FileNotFoundException DOCUMENT ME!
     */
    public static void writeRTCfile(final File outputFile) throws IOException, FileNotFoundException {

        if ( !outputFile.exists()) {

            try {
                outputFile.createNewFile();
            } catch (final IOException ioe) {
                throw ioe;
            }
        }

        if ( !outputFile.canWrite()) {
            throw new FileNotFoundException("Cannot write to " + outputFile.getAbsolutePath());
        }

        FileWriter RTCwriter;

        try {
            RTCwriter = new FileWriter(outputFile, false);
            RTCwriter.write(DICOM_RTC_Generator.genRTCpackage);
            RTCwriter.write("\n");
            RTCwriter.write(DICOM_RTC_Generator.genRTCwarning);
            RTCwriter.write("\n");
            RTCwriter.write(DICOM_RTC_Generator.genRTCimports);
            RTCwriter.write("\n");
            RTCwriter.write(DICOM_RTC_Generator.genRTCclassRTCEntry);
            RTCwriter.write("\n");
            RTCwriter.write(DICOM_RTC_Generator.genRTCjavadocComments);
            RTCwriter.write("");
            RTCwriter.write("public class " + DICOM_RTC_Generator.defaultRTCclassName + " { \n");

            final Hashtable entryHashVR = DICOM_RTC_Generator.hashVREntries();
            int i = 0;

            while (i < entryHashVR.size()) {
                RTCwriter.write("    public static final int " + entryHashVR.get(new Integer(i)) + " = " + i + ";\n");
                i++;
            }

            RTCwriter.write("\n");

            final Hashtable entryHashKeyword = DICOM_RTC_Generator.hashKeywordEntries();
            i = 0;

            while (i < entryHashKeyword.size()) {
                RTCwriter.write("    public static final int "
                        + (entryHashKeyword.get(new Integer(i)).toString().replace('-', '_')) + " = " + i + ";\n");
                i++;
            }

            RTCwriter.write("\n");
            RTCwriter.write(DICOM_RTC_Generator.genRTCclassRTCdefineHashtables);
            RTCwriter.write("\n");
            RTCwriter.write(DICOM_RTC_Generator.genRTCclassRTCarrayDef);

            final Hashtable classEntryHash = DICOM_RTC_Generator.hashRTCentries(entryHashKeyword);
            i = 1;

            while (i < (classEntryHash.size() - 1)) {
                RTCwriter.write("        " + classEntryHash.get(new Integer(i)) + ",");
                RTCwriter.write("\n");
                i++;
            }

            RTCwriter.write("        " + classEntryHash.get(new Integer(i)) + "\n");
            RTCwriter.write("    };  // End RTCEntry[] \n");
            RTCwriter.write("\n");

            RTCwriter.write(DICOM_RTC_Generator.genRTCclassRTCtypecodeComments);
            RTCwriter.write("    public static String getTypeCodeName" + "(int dd_type) { \n");
            RTCwriter.write("        String s = null;\n");
            RTCwriter.write("        int i = getIndex(dd_type); \n");
            RTCwriter.write("\n");
            RTCwriter.write("        if (i >= 0) { \n");
            RTCwriter.write("            if (!typeCodeNamesFilled) { \n");
            i = 1;

            while (i < entryHashVR.size()) {
                final Integer count = new Integer(i);
                final String typeString = (String) entryHashVR.get(count);
                final String vrString = "\"" + ((String) entryHashVR.get(count)).substring(5) + "\"";

                RTCwriter.write("                " + "typeCodeNames.put(new Integer(" + typeString + "), " + vrString
                        + "); \n");
                i++;
            }

            RTCwriter.write("                typeCodeNamesFilled = true; \n");
            RTCwriter.write("            } \n");
            RTCwriter.write(DICOM_RTC_Generator.genRTCclassRTCsetRTCcodeName);
            RTCwriter.write("        } \n");
            RTCwriter.write(DICOM_RTC_Generator.genRTCclassRTCsetUnkownRTCcodeName);
            RTCwriter.write("        return s;\n    } \n");

            RTCwriter.write(DICOM_RTC_Generator.genRTCclassRTCaddTypeHashComments);
            RTCwriter.write(DICOM_RTC_Generator.genRTCclassRTCaddTypeHashMethod);
            RTCwriter.write("\n");
            RTCwriter.write(DICOM_RTC_Generator.genRTCclassRTCstaticSearchGroup);
            RTCwriter.write("\n");
            RTCwriter.write(DICOM_RTC_Generator.genRTCclassRTCgetIndexComments);
            RTCwriter.write(DICOM_RTC_Generator.genRTCclassRTCgetIndexMethod);
            RTCwriter.write("\n");
            RTCwriter.write(DICOM_RTC_Generator.genRTCclassRTCunknownDDTypeComments);
            RTCwriter.write(DICOM_RTC_Generator.genRTCclassRTCunknownDDTypeMethod);
            RTCwriter.write("\n");
            RTCwriter.write(DICOM_RTC_Generator.genRTCclassRTCgetGroupComments);
            RTCwriter.write(DICOM_RTC_Generator.genRTCclassRTCgetGroupMethod);
            RTCwriter.write("\n");
            RTCwriter.write(DICOM_RTC_Generator.genRTCclassRTCgetElementComments);
            RTCwriter.write(DICOM_RTC_Generator.genRTCclassRTCgetElementMethod);
            RTCwriter.write("\n");
            RTCwriter.write(DICOM_RTC_Generator.genRTCclassRTCgetTypeCodeComments);
            RTCwriter.write(DICOM_RTC_Generator.genRTCclassRTCgetTypeCodeMethod);
            RTCwriter.write("\n");
            RTCwriter.write(DICOM_RTC_Generator.genRTCclassRTCgetDescriptionComments);
            RTCwriter.write(DICOM_RTC_Generator.genRTCclassRTCgetDescriptionMethod);

            RTCwriter.write("}  // end RTC\n");

            // write to disk
            RTCwriter.flush();
            RTCwriter.close();
        } catch (final IOException ioe) {
            throw ioe;
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    private static Hashtable hashKeywordEntries() {
        final Hashtable tagTable = DicomDictionary.getDicomTagTable();

        FileDicomTagInfo tag;
        int i = 1;

        final Hashtable hashKeyword = new Hashtable(tagTable.size());
        final Enumeration e = tagTable.elements();

        while (e.hasMoreElements()) {
            tag = (FileDicomTagInfo) e.nextElement();
            hashKeyword.put(new Integer(i), "DD_" + tag.getKeyword());
            i++;
        }

        hashKeyword.put(new Integer(i), "DD_LASTSEARCHED");
        hashKeyword.put(new Integer(0), "DD_UNDEFINED");

        return hashKeyword;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param kwEntries DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    private static Hashtable hashRTCentries(final Hashtable kwEntries) {
        final Hashtable tagTable = DicomDictionary.getDicomTagTable();

        FileDicomTagInfo tag;
        int i = 0;

        final Hashtable hashRTCentries = new Hashtable(tagTable.size());
        final Enumeration e = tagTable.elements();

        while (e.hasMoreElements()) {
            final Integer count = new Integer(i);
            tag = (FileDicomTagInfo) e.nextElement();
            hashRTCentries.put(count, "new RTCEntry(0x0" + Integer.toHexString(tag.getKey().getGroupNumber()) + ", 0x0"
                    + Integer.toHexString(tag.getKey().getElementNumber()) + ", " + "TYPE_"
                    + tag.getValueRepresentation() + ", " + "\"" + tag.getName() + "\"" + ", "
                    + ((String) kwEntries.get(new Integer(i + 1))).replace('-', '_') + ")");
            i++;
        }

        return hashRTCentries;
    }

    /**
     * Builds a hashtable of <code>"TYPE_"+FileDicomTag.getVR</code>.
     * 
     * @return DOCUMENT ME!
     */
    private static Hashtable hashVREntries() {
        final Hashtable tagTable = DicomDictionary.getDicomTagTable();

        FileDicomTagInfo tag;

        int i = 1;

        final Hashtable hashVR = new Hashtable(tagTable.size());
        final Enumeration e = tagTable.elements();

        hashVR.put(new Integer(0), "TYPE_UNKNOWN");

        while (e.hasMoreElements()) {
            tag = (FileDicomTagInfo) e.nextElement();

            if ( !hashVR.contains("TYPE_" + tag.getValueRepresentation())) {
                hashVR.put(new Integer(i), "TYPE_" + tag.getValueRepresentation());
                i++;
            }
        }

        return hashVR;
    }
}
