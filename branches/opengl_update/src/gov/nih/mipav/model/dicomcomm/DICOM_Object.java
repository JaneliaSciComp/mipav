package gov.nih.mipav.model.dicomcomm;


import java.util.*;


/**
 * The DICOM data object for storing DICOM tags (i.e. groups, elements).
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
 */
public class DICOM_Object {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** A hash table of the DICOM groups */
    protected Hashtable groups = new Hashtable();

    /** Flag used to support processing of GroupLength tags (XXXX, 0000) */
    private boolean popGroupLength = true;

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Makes length even.
     * 
     * @param length Value to make even
     * 
     * @return The even value
     */
    public static final int makeEven(int length) {

        if ( (length % 2) != 0) {
            length++;
        } // make even

        return (length);
    }

    public void finalize() {
        groups.clear();
        groups = null;
    }

    /**
     * Removes all VRs from hash table.
     */
    public final void clear() {
        groups.clear();
    }

    /**
     * Makes a deep copy of the DICOM_DataObject.
     * 
     * @return A deep copy of the DICOM_DataObject.
     */
    public DICOM_Object copy() {
        DICOM_Object newddo;
        Object obj;
        GroupElements ge;
        Integer element;

        newddo = new DICOM_Object();

        for (final Enumeration e1 = groups.keys(); e1.hasMoreElements();) {
            obj = e1.nextElement();
            ge = (GroupElements) groups.get(obj);

            for (final Enumeration e2 = ge.keys(); e2.hasMoreElements();) {
                element = (Integer) e2.nextElement();
                newddo.push( ((DICOM_VR) (ge.get(element))).copy());
            }
        }

        return (newddo);
    }

    /**
     * Retrieves an integer (16 bit) value from a DICOM object.
     * 
     * @param ddType The DICOM data type to get.
     * 
     * @return the 16 bit integer value
     */
    public final int getInt16(final int ddType) {
        int returnVal = 0;
        final DICOM_VR vr = getVR(DICOM_RTC.getGroup(ddType), DICOM_RTC.getElement(ddType));

        if (vr != null) {

            if (vr.data.length == 2) {
                returnVal = DICOM_Comms.bufferToInt16(vr.data, 0, DICOM_Comms.LITTLE_ENDIAN);
            }
        }

        return (returnVal);
    }

    /**
     * Retrieves a string value from a DICOM object.
     * 
     * @param ddType The DICOM data type to get.
     * 
     * @return the string value
     */
    public final String getStr(final int ddType) {
        String s = getVRString(DICOM_RTC.getGroup(ddType), DICOM_RTC.getElement(ddType));

        if (s == null) {
            return (s = "");
        } else {
            return (s.trim());
        }
    }

    /**
     * Returns the string representation of the VR.
     * 
     * @param group Group value
     * @param element Element value
     * 
     * @return The string representation VR for the group and element
     */
    public final String getVRString(final int group, final int element) {
        String returnval = null;
        final DICOM_VR vr = getVR(group, element);

        if (vr != null) {
            returnval = DICOM_Util.unpadStringVal(vr.data);
        }

        return (returnval);
    }

    /**
     * Pops a VR of the list.
     * 
     * @return vr The VR pop of the list.
     */
    public DICOM_VR pop() {

        int lowgroup = 65536;
        int lowelement = 65536;
        int tmpInt;
        GroupElements grpElement;

        if (groups.size() == 0) {
            popGroupLength = true;

            return (null);
        }

        // look for lowest in Group
        for (final Enumeration g = groups.keys(); g.hasMoreElements();) {
            tmpInt = ((Integer) g.nextElement()).intValue();

            if (tmpInt < lowgroup) {
                lowgroup = tmpInt;
            }
        }

        final Integer lowGroup = new Integer(lowgroup);
        grpElement = (GroupElements) groups.get(lowGroup);

        if (popGroupLength == true) {
            popGroupLength = false;

            return (new DICOM_VR(lowgroup, 0x0000, grpElement.grpLength, 4));
        }

        // look for lowest element in group
        for (final Enumeration e = grpElement.keys(); e.hasMoreElements();) {
            tmpInt = ((Integer) e.nextElement()).intValue();

            if (tmpInt < lowelement) {
                lowelement = tmpInt;
            }
        }

        final Integer lowElement = new Integer(lowelement);
        final DICOM_VR vr = (DICOM_VR) grpElement.get(lowElement);
        grpElement.remove(lowElement);

        if (grpElement.size() == 0) {
            groups.remove(lowGroup);
            popGroupLength = true;
        }

        return (vr);
    }

    /**
     * Adds DICOM_VR to this list (object).
     * 
     * @param vr the vr to be added to the list
     */
    public void push(final DICOM_VR vr) {
        DICOM_VR oldvr;

        if (vr == null) {
            return;
        }

        if (vr.element == 0x0000) {
            return;
        } // don't push onto the table -
        // group lengths automatically calculated

        final Integer group = new Integer(vr.group);
        final Integer element = new Integer(vr.element);
        GroupElements ge;

        ge = (GroupElements) groups.get(group);

        if (ge == null) {
            ge = new GroupElements();
            groups.put(group, ge);
        } else {
            oldvr = (DICOM_VR) ge.get(element);

            if (oldvr != null) {
                ge.grpLength -= DICOM_Object.makeEven(oldvr.data.length) + 8;
            }
        }

        ge.put(element, vr);
        ge.grpLength += DICOM_Object.makeEven(vr.data.length) + 8;
    }

    /**
     * Sets an integer (16 bit) value in a DICOM object.
     * 
     * @param ddType DOCUMENT ME!
     * @param val the 16 bit integer value
     */
    public final void setInt16(final int ddType, final int val) {
        push(new DICOM_VR(DICOM_RTC.getGroup(ddType), DICOM_RTC.getElement(ddType), val, 2));
    }

    /**
     * Sets a string value in a DICOM object.
     * 
     * @param ddType DOCUMENT ME!
     * @param s the string value
     */
    public final void setStr(final int ddType, final String s) {

        // Should I be making sure its even and padding ?
        push(new DICOM_VR(DICOM_RTC.getGroup(ddType), DICOM_RTC.getElement(ddType), new String(s), ddType));
    }

    /**
     * Used for DICOM data objects.
     * 
     * @return String of important aspects of the data object
     */
    public String toString() {
        String returnString = "";
        String str;

        str = getStr(DICOM_RTC.DD_PatientName);
        if (str != null) {
            returnString += ("|" + str);
        }

        str = getStr(DICOM_RTC.DD_PatientAge);
        if (str != null) {
            returnString += ("|" + str);
        }

        str = getStr(DICOM_RTC.DD_PatientSex);
        if (str != null) {
            returnString += ("|" + str);
        }

        str = getStr(DICOM_RTC.DD_PatientID);
        if (str != null) {
            returnString += ("|" + str);
        }

        str = getStr(DICOM_RTC.DD_StudyID);
        if (str != null) {
            returnString += ("|" + str);
        }

        str = getStr(DICOM_RTC.DD_Modality);
        if (str != null) {
            returnString += ("|" + str);
        }

        str = getStr(DICOM_RTC.DD_StudyDescription);
        if (str != null) {
            returnString += ("|" + str);
        }

        str = getStr(DICOM_RTC.DD_AdditionalPatientHistory);
        if (str != null) {
            returnString += ("|" + str);
        }

        str = getStr(DICOM_RTC.DD_ReferringPhysicianName);
        if (str != null) {
            returnString += ("|" + str);
        }

        str = getStr(DICOM_RTC.DD_OperatorName);
        if (str != null) {
            returnString += ("|" + str);
        }

        str = getStr(DICOM_RTC.DD_SeriesNumber);
        if (str != null) {
            returnString += ("|" + str);
        }

        str = getStr(DICOM_RTC.DD_ImageIndex);
        if (str != null) {
            returnString += ("|" + str);
        }

        str = getStr(DICOM_RTC.DD_ContentDate);
        if (str != null) {
            returnString += ("|" + str);
        }

        str = getStr(DICOM_RTC.DD_ContentTime);
        if (str != null) {
            returnString += ("|" + str);
        }

        return (returnString);
    }

    /**
     * Creates a description of the entire DICOM_VR list.
     * 
     * @param s The str to append the debug string
     * 
     * @return The debug string
     */
    public String toString(final String s) {
        String str;
        final DICOM_Object tmpObjectList = new DICOM_Object();
        DICOM_VR vr;

        str = "***** DICOM Object List " + s + " Begin *****\n";

        while ( (vr = pop()) != null) {
            tmpObjectList.push(vr);
            str += "       " + vr.toString(s) + "\n";
        }

        str += "***** DICOM Object List " + s + " End ***** \n\n";

        while ( (vr = tmpObjectList.pop()) != null) {
            push(vr);
        }

        return (str);
    }

    /**
     * Gets the value representation.
     * 
     * @param grp Group value
     * @param elem Element value
     * 
     * @return the VR for the group and element
     */
    private DICOM_VR getVR(final int grp, final int elem) {
        final Integer group = new Integer(grp);
        final Integer element = new Integer(elem);
        GroupElements grpElement;

        grpElement = (GroupElements) groups.get(group);

        if (grpElement == null) {
            return (null);
        }

        if (elem == 0x0000) {
            return (new DICOM_VR(grp, elem, grpElement.grpLength, 4));
        }

        return ((DICOM_VR) grpElement.get(element));
    }

    // ~ Inner Classes
    // --------------------------------------------------------------------------------------------------

    /**
     * Simple extension of the Hastable for use a GroupElements object.
     */
    private class GroupElements extends Hashtable {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -3780517111813962572L;

        /** Stores length of group. */
        public int grpLength = 0;
    }
}
