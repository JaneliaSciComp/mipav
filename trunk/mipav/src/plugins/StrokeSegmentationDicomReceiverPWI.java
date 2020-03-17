/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is part of dcm4che, an implementation of DICOM(TM) in
 * Java(TM), hosted at https://github.com/dcm4che.
 *
 * The Initial Developer of the Original Code is
 * Agfa Healthcare.
 * Portions created by the Initial Developer are Copyright (C) 2011
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * See @authors listed below
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK ***** */


import gov.nih.mipav.model.file.FileInfoDicom;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.components.WidgetFactory;

import java.io.*;
import java.security.GeneralSecurityException;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.locks.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.activation.*;
import javax.mail.*;
import javax.mail.internet.*;

import org.dcm4che3.data.Tag;
import org.apache.commons.io.FileUtils;
import org.dcm4che3.data.Attributes;
import org.dcm4che3.data.VR;
import org.dcm4che3.io.DicomInputStream;
import org.dcm4che3.io.DicomOutputStream;
import org.dcm4che3.io.DicomInputStream.IncludeBulkData;
import org.dcm4che3.net.*;
import org.dcm4che3.net.pdu.PresentationContext;
import org.dcm4che3.net.service.BasicCEchoSCP;
import org.dcm4che3.net.service.BasicCStoreSCP;
import org.dcm4che3.net.service.DicomServiceException;
import org.dcm4che3.net.service.DicomServiceRegistry;
import org.dcm4che3.util.AttributesFormat;
import org.dcm4che3.util.SafeClose;
import org.dcm4che3.util.TagUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


/**
 * Adapted from dcm4che's StoreSCP tool to catch files, sort them, and run them through the stroke segmentation plugin.
 */
public class StrokeSegmentationDicomReceiverPWI {

    private static final Logger LOG = LoggerFactory.getLogger(StrokeSegmentationDicomReceiverPWI.class);

    private static ResourceBundle rb = ResourceBundle.getBundle("org.dcm4che3.tool.storescp.messages");
    private static final String PART_EXT = ".part";

    private final Device device = new Device("storescp");
    private final ApplicationEntity ae = new ApplicationEntity("*");
    private final Connection conn = new Connection();
    private File storageDir;
    private AttributesFormat filePathFormat;
    private int status;
    
    private final Lock fileListLock = new ReentrantLock();
    
    /** Table of threads that keep track of the incoming data, both in wait time and number of slices. Indexed by the base directory the incoming files are put into. */
    private Hashtable<String, CoreToolProcessingThread> coreToolThreadTable = new Hashtable<String, CoreToolProcessingThread>();
    
    private final BasicCStoreSCP cstoreSCP = new BasicCStoreSCP("*") {

        @Override
        protected void store(Association as, PresentationContext pc,
                Attributes rq, PDVInputStream data, Attributes rsp)
                throws IOException {
            rsp.setInt(Tag.Status, VR.US, status);
            if (storageDir == null)
                return;
            
            fileListLock.lock();
            try {
                if (!addedCloseListener) {
                    as.addAssociationListener(new AssociationClose());
                    receivedFileList = new Vector<File>();
                    addedCloseListener = true;
                }
    
                String cuid = rq.getString(Tag.AffectedSOPClassUID);
                String iuid = rq.getString(Tag.AffectedSOPInstanceUID);
                String tsuid = pc.getTransferSyntax();
                File file = new File(storageDir, iuid + PART_EXT);
                try {
                    storeTo(as, as.createFileMetaInformation(iuid, cuid, tsuid),
                            data, file);
                    final String outFileName = new String(filePathFormat == null ? iuid : filePathFormat.format(parse(file)));
                    final File outFile = new File(storageDir, outFileName);
                    renameTo(as, file, outFile);
                    
                    log("Received file: " + outFileName);
                    receivedFileList.add(outFile);
                } catch (Exception e) {
                    deleteFile(as, file);
                    throw new DicomServiceException(Status.ProcessingFailure, e);
                }
            } finally {
                fileListLock.unlock();
            }
        }

    };
    
    private boolean addedCloseListener = false;
    
    private Vector<File> receivedFileList;
    
    private String serverIP;
    private int serverPort;
    private String serverAE;
    
    private String reportDir;
    
    private boolean doEmailReport;
    
    private static final String reportCidImgName = "core-image-";
    private static final String reportCidBase = "cid:" + reportCidImgName;
    
    private WidgetFactory.ScrollTextArea logOutputArea;
    
    private static final String configFileName = "stroke_seg_listener.properties";
    
    private String emailFrom;
    private String emailTo;
    private String emailUsername;
    private String emailPassword;
    private String emailHost;
    private String emailPort;
    
    private int minExpectedSlices;
    
    private int[] minExpectedSlicesPWI;
    private int maxWaitTime;

    // TODO new storage format
    
    private String storageFilePathFormat = "incoming/{00080020}/{00080020}.{00080030}/{00080021}.{00080031}/{00200011}.{00200012}.{00200013}.dcm";
    
//    private String storageFilePathFormat = "{00080020}/{00080020}.{00080030}/{00200011}.{00200012}.{00200013}.dcm";
    
//    private String storageFilePathFormat = "{0020000D}/{0020000E}/{00080008}/{00080018}.dcm";
    
    public StrokeSegmentationDicomReceiverPWI(final String ip, final int port, final String curAE, final String outputDir, final String reportDir, final int numSlices, final int[] numSlicesPWI, final int maxWait, final boolean doEmail, final WidgetFactory.ScrollTextArea area) throws IOException {
        serverIP = ip;
        serverPort = port;
        serverAE = curAE;
        
        doEmailReport = doEmail;
        
        logOutputArea = area;
        
        minExpectedSlices = numSlices;
        
        minExpectedSlicesPWI = numSlicesPWI;
        maxWaitTime = maxWait;
        
        conn.setBindAddress(serverIP);
        conn.setPort(serverPort);
        
        device.setDimseRQHandler(createServiceRegistry());
        device.addConnection(conn);
        device.addApplicationEntity(ae);
        ae.setAssociationAcceptor(true);
        ae.addConnection(conn);
        
        ae.setAETitle(serverAE);
        
        conn.setReceivePDULength(Connection.DEF_MAX_PDU_LENGTH);
        conn.setSendPDULength(Connection.DEF_MAX_PDU_LENGTH);
        conn.setMaxOpsInvoked(0);
        conn.setMaxOpsPerformed(0);
        conn.setPackPDV(false);
        conn.setConnectTimeout(0);
        conn.setRequestTimeout(0);
        conn.setAcceptTimeout(0);
        conn.setReleaseTimeout(0);
        conn.setResponseTimeout(0);
        conn.setRetrieveTimeout(0);
        conn.setIdleTimeout(0);
        conn.setSocketCloseDelay(Connection.DEF_SOCKETDELAY);
        conn.setSendBufferSize(0);
        conn.setReceiveBufferSize(0);
        conn.setTcpNoDelay(false);
//        configureTLS(conn, cl);
        
        setStatus(0);
        
        ae.addTransferCapability(new TransferCapability(null, "*", TransferCapability.Role.SCP, "*"));
        
        setStorageDirectory(new File(outputDir));
        
        this.reportDir = reportDir;
        
        setStorageFilePathFormat(storageFilePathFormat);
        
        ExecutorService executorService = Executors.newCachedThreadPool();
        ScheduledExecutorService scheduledExecutorService = Executors.newSingleThreadScheduledExecutor();
        device.setScheduledExecutor(scheduledExecutorService);
        device.setExecutor(executorService);
        try {
            device.bindConnections();
        } catch (GeneralSecurityException e) {
            e.printStackTrace();
        }
    }

    private void storeTo(Association as, Attributes fmi, 
            PDVInputStream data, File file) throws IOException  {
        LOG.info("{}: M-WRITE {}", as, file);
        file.getParentFile().mkdirs();
        DicomOutputStream out = new DicomOutputStream(file);
        try {
            out.writeFileMetaInformation(fmi);
            data.copyTo(out);
        } finally {
            SafeClose.close(out);
        }
    }

    private static void renameTo(Association as, File from, File dest)
            throws IOException {
        LOG.info("{}: M-RENAME {} to {}", as, from, dest);
        if (!dest.getParentFile().mkdirs())
            dest.delete();
        if (!from.renameTo(dest))
            throw new IOException("Failed to rename " + from + " to " + dest);
    }

    private static Attributes parse(File file) throws IOException {
        DicomInputStream in = new DicomInputStream(file);
        try {
            in.setIncludeBulkData(IncludeBulkData.NO);
            return in.readDataset(-1, Tag.PixelData);
        } finally {
            SafeClose.close(in);
        }
    }

    private static void deleteFile(Association as, File file) {
        if (file.delete())
            LOG.info("{}: M-DELETE {}", as, file);
        else
            LOG.warn("{}: M-DELETE {} failed!", as, file);
    }

    private DicomServiceRegistry createServiceRegistry() {
        DicomServiceRegistry serviceRegistry = new DicomServiceRegistry();
        serviceRegistry.addDicomService(new BasicCEchoSCP());
        serviceRegistry.addDicomService(cstoreSCP);
        return serviceRegistry;
    }

    public void setStorageDirectory(File storageDir) {
        if (storageDir != null)
            storageDir.mkdirs();
        this.storageDir = storageDir;
    }

    public void setStorageFilePathFormat(String pattern) {
        this.filePathFormat = new AttributesFormat(pattern);
    }

    public void setStatus(int status) {
        this.status = status;
    }

    private class AssociationClose implements AssociationListener {
        public void onClose(Association association) {
            fileListLock.lock();
            try {
                log("Received association close request");
                
                boolean foundADC = false;
                boolean foundDWI = false;
                boolean foundPWI = false;
                
                Vector<File> adcFiles = new Vector<File>();
                Vector<File> dwiFiles = new Vector<File>();
                Vector<File> pwiFiles = new Vector<File>();
                
                Vector<Attributes> adcAttrList = new Vector<Attributes>();
                Vector<Attributes> dwiAttrList = new Vector<Attributes>();
                Vector<Attributes> pwiAttrList = new Vector<Attributes>();
                
                File baseOutputDir = null;
                String lastnameInitial = null;
                String studyDate = null;
                String studyTime = null;
                
                boolean foundRegSeries = false;
                
                for (File file : receivedFileList) {
                    try {
                        Attributes attr = parse(file);
                        
                        final String[] imageTypes = attr.getStrings(TagUtils.toTag(0x0008, 0x0008));
                        
                        if (lastnameInitial == null) {
                            lastnameInitial = getInitialFromName(attr.getString(TagUtils.toTag(0x0010, 0x0010)));
                        }
                        
                        if (baseOutputDir == null) {
                            studyDate = attr.getString(TagUtils.toTag(0x0008, 0x0020));
                            studyTime = attr.getString(TagUtils.toTag(0x0008, 0x0030));
                            
                            baseOutputDir = new File(storageDir + File.separator + studyDate + File.separator + studyDate + "." + studyTime + "_" + lastnameInitial + File.separator);
                            if (!baseOutputDir.exists()) {
                                log("Creating output directory: " + baseOutputDir);
                                baseOutputDir.mkdirs();
                            }
                        }
                        
                        final String seriesDesc = attr.getString(TagUtils.toTag(0x0008, 0x103E));
                        final String protocolName = attr.getString(TagUtils.toTag(0x0018, 0x1030));
                        
                        if (!foundRegSeries && PlugInDialogStrokeSegmentationPWI.isRegisteredVol(seriesDesc, protocolName)) {
                            foundRegSeries = true;
                        }
                        
                        for (String val : imageTypes) {
                            if (PlugInDialogStrokeSegmentationPWI.isADC(val)) {
                                adcFiles.add(file);
                                adcAttrList.add(attr);
                                foundADC = true;
                                break;
                            } else if (PlugInDialogStrokeSegmentationPWI.isDWI(val)) {
                                dwiFiles.add(file);
                                dwiAttrList.add(attr);
                                foundDWI = true;
                                break;
                            } else if (PlugInDialogStrokeSegmentationPWI.isPWI(val)) {
                                pwiFiles.add(file);
                                pwiAttrList.add(attr);
                                foundPWI = true;
                                break;
                            }
                        }
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }
                
                // check that a previous segmentation hasn't been done on this set of scans
                File[] outputDirFiles = baseOutputDir.listFiles();
                for (File file : outputDirFiles) {
                    if (file.getName().equalsIgnoreCase("core_seg_report.html")) {
                        log("New data received for previously segmented subject.  Not running the segmentation again.");
                        return;
                    }
                }
                
                // if we haven't received data from this subject/study before, created a new monitor thread.  otherwise get it so that we can add file counts
                String baseOutputDirStr = baseOutputDir.getAbsolutePath();
                CoreToolProcessingThread curDatasetThread;
                if (!coreToolThreadTable.containsKey(baseOutputDirStr)) {
                	curDatasetThread = new CoreToolProcessingThread(baseOutputDirStr, maxWaitTime, minExpectedSlicesPWI, minExpectedSlices);
                	coreToolThreadTable.put(baseOutputDirStr, curDatasetThread);
                	Thread thread = new Thread(curDatasetThread);
                	thread.start();
                } else {
                	curDatasetThread = coreToolThreadTable.get(baseOutputDirStr);
                }

                // TODO - PWI support
                // on getting new subject scan, start timer to wait for PWI
                // on timer expiration, check that minimum number of PWI slices is met
                // if enough slices, check that ADC/DWI are done transferring and run PWI-incorporating algorithm
                // if not enough slices, check that ADC/DWI are done transferring and run non-PWI algorithm
                // TODO - maybe also trigger if specific number of slices is hit exactly - 1600, 2400?
                
                if (foundADC) {
                    // check for Reg series in the newly received files
                    boolean isNewReg = false;
                    Vector<File> adcRegFiles = new Vector<File>();
                    
                    for (int i = 0; i < adcFiles.size(); i++) {
                        final Attributes attr = adcAttrList.get(i);
                        final String seriesDesc = attr.getString(TagUtils.toTag(0x0008, 0x103E));
                        final String protocolName = attr.getString(TagUtils.toTag(0x0018, 0x1030));
                        
                        if (PlugInDialogStrokeSegmentationPWI.isRegisteredVol(seriesDesc, protocolName)) {
                            adcRegFiles.add(adcFiles.get(i));
                        }
                    }
                    
                    if (adcRegFiles.size() > 0) {
                        adcFiles = adcRegFiles;
                        isNewReg = true;
                    }
                    
                    // check for previous transfer
                    File adcDirFile = new File(baseOutputDir + File.separator + "ADC");
                    if (adcDirFile.exists()) {
                        log("Found previously received ADC volume in output directory, but also received new ADC volume.");
                        
                        File prevFile = adcDirFile.listFiles()[0];
                        Attributes prevAttr = null;
                        try {
                            prevAttr = parse(prevFile);
                        } catch (IOException e) {
                            e.printStackTrace();
                        }
                        
                        Attributes newAttr = adcAttrList.get(0);
                        
                        boolean isPrevReg = isRegisteredVol(prevAttr);
                        
                        final String prevStudyNum = prevAttr.getString(TagUtils.toTag(0x0020, 0x0011));
                        final String newStudyNum = newAttr.getString(TagUtils.toTag(0x0020, 0x0011));
                        
                        if (isNewReg && isPrevReg && prevStudyNum.equalsIgnoreCase(newStudyNum)) {
                            // both Reg w/ same study num - merge
                            log("Merging new ADC files with previously received volume.");
                            
                            for (File file : adcFiles) {
                                try {
                                    renameTo(association, file, new File(adcDirFile.getAbsolutePath() + File.separator + file.getName()));
                                } catch (IOException e) {
                                    e.printStackTrace();
                                }
                            }
                        } else if (!isNewReg && !isPrevReg && prevStudyNum.equalsIgnoreCase(newStudyNum)) {
                            // both not Reg w/ same study num - merge
                            log("Merging new ADC files with previously received volume.");
                            
                            for (File file : adcFiles) {
                                try {
                                    renameTo(association, file, new File(adcDirFile.getAbsolutePath() + File.separator + file.getName()));
                                } catch (IOException e) {
                                    e.printStackTrace();
                                }
                            }
                        } else if (isNewReg && !isPrevReg) {
                            // move prev out of the way and use new files only
                            log("Preferring newly received ADC volume.");

                            // clean-up prev ADC dir
                            File backupDir = new File(adcDirFile.getAbsolutePath() + "_old_" + System.currentTimeMillis());
                            log("Moving to old ADC volume to " + backupDir);
                            boolean success = adcDirFile.renameTo(backupDir);
                            if (!success) {
                                log("Failed to move previous ADC data to : " + backupDir);
                            }
                            curDatasetThread.resetAdcSliceCount();
                            
                            for (File file : adcFiles) {
                                try {
                                    renameTo(association, file, new File(adcDirFile.getAbsolutePath() + File.separator + file.getName()));
                                } catch (IOException e) {
                                    e.printStackTrace();
                                }
                            }
                        } else if (!isNewReg && isPrevReg) {
                            // disregard new files and only use prev files
                            log("Preferring previously received ADC volume.");
                            
                            // remove all the new files from our list
                            adcFiles.clear();
                        } else {
                            // TODO non-matching study numbers w/ same Reg status - what should be done?
                            log("Both previously received volume and new ADC files received, but study numbers do not match (old: " + prevStudyNum + " new: " + newStudyNum + ")");
                        }
                    } else {
                        // no prev transfer - use new files and move them to processing dir
                        log("Received new ADC volume.  No previous ADC transfer for this subject/study.");

                        if (!adcDirFile.mkdir()) {
                            log("Failed to create directory: " + adcDirFile);
                        } else {
                            for (File file : adcFiles) {
                                try {
                                    renameTo(association, file, new File(adcDirFile.getAbsolutePath() + File.separator + file.getName()));
                                } catch (IOException e) {
                                    e.printStackTrace();
                                }
                            }
                        }
                    }
                }
                
                curDatasetThread.addToAdcSliceCount(adcFiles.size());
                
                if (foundDWI) {
                    // check for Reg series in the newly received files
                    boolean isNewReg = false;
                    Vector<File> dwiRegFiles = new Vector<File>();
                    
                    for (int i = 0; i < dwiFiles.size(); i++) {
                        final Attributes attr = dwiAttrList.get(i);
                        final String seriesDesc = attr.getString(TagUtils.toTag(0x0008, 0x103E));
                        final String protocolName = attr.getString(TagUtils.toTag(0x0018, 0x1030));
                        
                        if (PlugInDialogStrokeSegmentationPWI.isRegisteredVol(seriesDesc, protocolName)) {
                            dwiRegFiles.add(dwiFiles.get(i));
                        }
                    }
                    
                    if (dwiRegFiles.size() > 0) {
                        dwiFiles = dwiRegFiles;
                        isNewReg = true;
                    }
                    
                    // check for previous transfer
                    File dwiDirFile = new File(baseOutputDir + File.separator + "DWI");
                    if (dwiDirFile.exists()) {
                        log("Found previously received DWI volume in output directory, but also received new DWI volume.");
                        
                        File prevFile = dwiDirFile.listFiles()[0];
                        Attributes prevAttr = null;
                        try {
                            prevAttr = parse(prevFile);
                        } catch (IOException e) {
                            e.printStackTrace();
                        }
                        
                        Attributes newAttr = dwiAttrList.get(0);
                        
                        boolean isPrevReg = isRegisteredVol(prevAttr);
                        
                        final String prevStudyNum = prevAttr.getString(TagUtils.toTag(0x0020, 0x0011));
                        final String newStudyNum = newAttr.getString(TagUtils.toTag(0x0020, 0x0011));
                        
                        if (isNewReg && isPrevReg && prevStudyNum.equalsIgnoreCase(newStudyNum)) {
                            // both Reg w/ same study num - merge
                            log("Merging new DWI files with previously received volume.");
                            
                            for (File file : dwiFiles) {
                                try {
                                    renameTo(association, file, new File(dwiDirFile.getAbsolutePath() + File.separator + file.getName()));
                                } catch (IOException e) {
                                    e.printStackTrace();
                                }
                            }
                        } else if (!isNewReg && !isPrevReg && prevStudyNum.equalsIgnoreCase(newStudyNum)) {
                            // both not Reg w/ same study num - merge
                            log("Merging new DWI files with previously received volume.");
                            
                            for (File file : dwiFiles) {
                                try {
                                    renameTo(association, file, new File(dwiDirFile.getAbsolutePath() + File.separator + file.getName()));
                                } catch (IOException e) {
                                    e.printStackTrace();
                                }
                            }
                        } else if (isNewReg && !isPrevReg) {
                            // move prev out of the way and use new files only
                            log("Preferring newly received DWI volume.");

                            // clean-up prev DWI dir
                            File backupDir = new File(dwiDirFile.getAbsolutePath() + "_old_" + System.currentTimeMillis());
                            log("Moving to old DWI volume to " + backupDir);
                            boolean success = dwiDirFile.renameTo(backupDir);
                            if (!success) {
                                log("Failed to move previous DWI data to : " + backupDir);
                            }
                            curDatasetThread.resetDwiSliceCount();
                            
                            for (File file : dwiFiles) {
                                try {
                                    renameTo(association, file, new File(dwiDirFile.getAbsolutePath() + File.separator + file.getName()));
                                } catch (IOException e) {
                                    e.printStackTrace();
                                }
                            }
                        } else if (!isNewReg && isPrevReg) {
                            // disregard new files and only use prev files
                            log("Preferring previously received DWI volume.");
                            
                            // remove all the new files from our list
                            dwiFiles.clear();
                        } else {
                            // TODO non-matching study numbers w/ same Reg status - what should be done?
                            log("Both previously received volume and new DWI files received, but study numbers do not match (old: " + prevStudyNum + " new: " + newStudyNum + ")");
                        }
                    } else {
                        // no prev transfer - use new files and move them to processing dir
                        log("Received new DWI volume.  No previous DWI transfer for this subject/study.");

                        if (!dwiDirFile.mkdir()) {
                            log("Failed to create directory: " + dwiDirFile);
                        } else {
                            for (File file : dwiFiles) {
                                try {
                                    renameTo(association, file, new File(dwiDirFile.getAbsolutePath() + File.separator + file.getName()));
                                } catch (IOException e) {
                                    e.printStackTrace();
                                }
                            }
                        }
                    }
                }
                
                curDatasetThread.addToDwiSliceCount(dwiFiles.size());
                
                if (foundPWI) {
                    // check for Reg series in the newly received files
                    boolean isNewReg = false;
                    Vector<File> pwiRegFiles = new Vector<File>();
                    
                    for (int i = 0; i < pwiFiles.size(); i++) {
                        final Attributes attr = pwiAttrList.get(i);
                        final String seriesDesc = attr.getString(TagUtils.toTag(0x0008, 0x103E));
                        final String protocolName = attr.getString(TagUtils.toTag(0x0018, 0x1030));
                        
                        if (PlugInDialogStrokeSegmentationPWI.isRegisteredVol(seriesDesc, protocolName)) {
                            pwiRegFiles.add(pwiFiles.get(i));
                        }
                    }
                    
                    if (pwiRegFiles.size() > 0) {
                        pwiFiles = pwiRegFiles;
                        isNewReg = true;
                    }
                    
                    // check for previous transfer
                    File pwiDirFile = new File(baseOutputDir + File.separator + "PWI");
                    if (pwiDirFile.exists()) {
                        log("Found previously received PWI volume in output directory, but also received new PWI volume.");
                        
                        File prevFile = pwiDirFile.listFiles()[0];
                        Attributes prevAttr = null;
                        try {
                            prevAttr = parse(prevFile);
                        } catch (IOException e) {
                            e.printStackTrace();
                        }
                        
                        Attributes newAttr = pwiAttrList.get(0);
                        
                        boolean isPrevReg = isRegisteredVol(prevAttr);
                        
                        final String prevStudyNum = prevAttr.getString(TagUtils.toTag(0x0020, 0x0011));
                        final String newStudyNum = newAttr.getString(TagUtils.toTag(0x0020, 0x0011));
                        
                        if (isNewReg && isPrevReg && prevStudyNum.equalsIgnoreCase(newStudyNum)) {
                            // both Reg w/ same study num - merge
                            log("Merging new PWI files with previously received volume.");
                            
                            for (File file : pwiFiles) {
                                try {
                                    renameTo(association, file, new File(pwiDirFile.getAbsolutePath() + File.separator + file.getName()));
                                } catch (IOException e) {
                                    e.printStackTrace();
                                }
                            }
                        } else if (!isNewReg && !isPrevReg && prevStudyNum.equalsIgnoreCase(newStudyNum)) {
                            // both not Reg w/ same study num - merge
                            log("Merging new PWI files with previously received volume.");
                            
                            for (File file : pwiFiles) {
                                try {
                                    renameTo(association, file, new File(pwiDirFile.getAbsolutePath() + File.separator + file.getName()));
                                } catch (IOException e) {
                                    e.printStackTrace();
                                }
                            }
                        } else if (isNewReg && !isPrevReg) {
                            // move prev out of the way and use new files only
                            log("Preferring newly received PWI volume.");

                            // clean-up prev PWI dir
                            File backupDir = new File(pwiDirFile.getAbsolutePath() + "_old_" + System.currentTimeMillis());
                            log("Moving to old PWI volume to " + backupDir);
                            boolean success = pwiDirFile.renameTo(backupDir);
                            if (!success) {
                                log("Failed to move previous PWI data to : " + backupDir);
                            }
                            curDatasetThread.resetPwiSliceCount();
                            
                            for (File file : pwiFiles) {
                                try {
                                    renameTo(association, file, new File(pwiDirFile.getAbsolutePath() + File.separator + file.getName()));
                                } catch (IOException e) {
                                    e.printStackTrace();
                                }
                            }
                        } else if (!isNewReg && isPrevReg) {
                            // disregard new files and only use prev files
                            log("Preferring previously received PWI volume.");
                            
                            // remove all the new files from our list
                            pwiFiles.clear();
                        } else {
                            // TODO non-matching study numbers w/ same Reg status - what should be done?
                            log("Both previously received volume and new PWI files received, but study numbers do not match (old: " + prevStudyNum + " new: " + newStudyNum + ")");
                        }
                    } else {
                        // no prev transfer - use new files and move them to processing dir
                        log("Received new PWI volume.  No previous PWI transfer for this subject/study.");

                        if (!pwiDirFile.mkdir()) {
                            log("Failed to create directory: " + pwiDirFile);
                        } else {
                            for (File file : pwiFiles) {
                                try {
                                    renameTo(association, file, new File(pwiDirFile.getAbsolutePath() + File.separator + file.getName()));
                                } catch (IOException e) {
                                    e.printStackTrace();
                                }
                            }
                        }
                    }
                }
                
                curDatasetThread.addToPwiSliceCount(pwiFiles.size());
                
//                // if we didn't find both ADC and DWI, check for previous transfer in output dir
//                if (baseOutputDir != null && !foundADC || !foundDWI || !foundPWI) {
//                    File[] outputDirFiles = baseOutputDir.listFiles();
//                    for (File file : outputDirFiles) {
//                        if (file.isDirectory()) { 
//                            if (file.getName().equalsIgnoreCase("ADC")) {
//                                foundOutputDirADC = true;
//                            } else if (file.getName().equalsIgnoreCase("DWI")) {
//                                foundOutputDirDWI = true;
//                            } else if (file.getName().equalsIgnoreCase("PWI")) {
//                                foundOutputDirPWI = true;
//                            }
//                        } else if (file.getName().equalsIgnoreCase("core_seg_report.html")) {
//                            log("New data received for previously segmented subject.  Not running the segmentation again.");
//                            alreadySegmented = true;
//                        }
//                    }
//                    
//                    if (foundADC && foundOutputDirADC) {
//                        log("Found previously received ADC volume in output directory, but also received new ADC volume.");
//                        
//                        File prevFile = new File(baseOutputDir + File.separator + "ADC").listFiles()[0];
//                        Attributes prevAttr = null;
//                        try {
//                            prevAttr = parse(prevFile);
//                        } catch (IOException e) {
//                            e.printStackTrace();
//                        }
//                        Attributes newAttr = adcAttrList.get(0);
//                        
//                        boolean isPrevReg = isRegisteredVol(prevAttr);
//                        boolean isNewReg = isRegisteredVol(newAttr);
//                        
//                        final String prevStudyNum = prevAttr.getString(TagUtils.toTag(0x0020, 0x0011));
//                        final String newStudyNum = newAttr.getString(TagUtils.toTag(0x0020, 0x0011));
//                        
//                        // prefer series marked as 'reg', but otherwise go with the new one
//                        if (!isNewReg && isPrevReg) {
//                            log("Preferring previously received ADC volume.");
//                            usePrevADC = true;
//                        } else if (isNewReg && !isPrevReg) {
//                            log("Preferring newly received ADC volume.");
//                            usePrevADC = false;
//                        } else if (prevStudyNum.equals(newStudyNum)) {
//                            log("Merging new ADC files with previously received volume.");
//                            mergeADC = true;
//                        } else {
//                            log("Preferring newly received ADC volume.");
//                            usePrevADC = false;
//                        }
//                    } else if (!foundADC && foundOutputDirADC) {
//                        log("Found previously received ADC volume in output directory.");
//                        usePrevADC = true;
//                    }
//                    
//                    if (foundDWI && foundOutputDirDWI) {
//                        log("Found previously received DWI volume in output directory, but also received new DWI volume.");
//                        
//                        File prevFile = new File(baseOutputDir + File.separator + "DWI").listFiles()[0];
//                        Attributes prevAttr = null;
//                        try {
//                            prevAttr = parse(prevFile);
//                        } catch (IOException e) {
//                            e.printStackTrace();
//                        }
//                        Attributes newAttr = dwiAttrList.get(0);
//                        
//                        boolean isPrevReg = isRegisteredVol(prevAttr);
//                        boolean isNewReg = isRegisteredVol(newAttr);
//                        
//                        final String prevStudyNum = prevAttr.getString(TagUtils.toTag(0x0020, 0x0011));
//                        final String newStudyNum = newAttr.getString(TagUtils.toTag(0x0020, 0x0011));
//                        
//                        // prefer series marked as 'reg', but otherwise go with the new one
//                        if (!isNewReg && isPrevReg) {
//                            log("Preferring previously received DWI volume.");
//                            usePrevDWI = true;
//                        } else if (isNewReg && !isPrevReg) {
//                            log("Preferring newly received DWI volume.");
//                            usePrevDWI = false;
//                        } else if (prevStudyNum.equals(newStudyNum)) {
//                            log("Merging new DWI files with previously received volume.");
//                            mergeDWI = true;
//                        } else {
//                            log("Preferring newly received DWI volume.");
//                            usePrevDWI = false;
//                        }
//                    } else if (!foundDWI && foundOutputDirDWI) {
//                        log("Found previously received DWI volume in output directory.");
//                        usePrevDWI = true;
//                    }
//                    
//                    if (foundPWI && foundOutputDirPWI) {
//                        log("Found previously received PWI volume in output directory, but also received new PWI volume.");
//                        
//                        File prevFile = new File(baseOutputDir + File.separator + "PWI").listFiles()[0];
//                        Attributes prevAttr = null;
//                        try {
//                            prevAttr = parse(prevFile);
//                        } catch (IOException e) {
//                            e.printStackTrace();
//                        }
//                        Attributes newAttr = pwiAttrList.get(0);
//                        
//                        boolean isPrevReg = isRegisteredVol(prevAttr);
//                        boolean isNewReg = isRegisteredVol(newAttr);
//                        
//                        final String prevStudyNum = prevAttr.getString(TagUtils.toTag(0x0020, 0x0011));
//                        final String newStudyNum = newAttr.getString(TagUtils.toTag(0x0020, 0x0011));
//                        
//                        // prefer series marked as 'reg', but otherwise go with the new one
//                        if (!isNewReg && isPrevReg) {
//                            log("Preferring previously received PWI volume.");
//                            usePrevPWI = true;
//                        } else if (isNewReg && !isPrevReg) {
//                            log("Preferring newly received PWI volume.");
//                            usePrevPWI = false;
//                        } else if (prevStudyNum.equals(newStudyNum)) {
//                            log("Merging new PWI files with previously received volume.");
//                            mergePWI = true;
//                        } else {
//                            log("Preferring newly received PWI volume.");
//                            usePrevPWI = false;
//                        }
//                    } else if (!foundPWI && foundOutputDirPWI) {
//                        log("Found previously received PWI volume in output directory.");
//                        usePrevPWI = true;
//                    }
//                }
//                
//                // move ADC and DWI files to their own dir under parent inside outputDir
//                if (foundADC && !usePrevADC) {
//                    log("Found ADC volume in completed transfer. Moving to " + baseOutputDir);
//                    
//                    File adcDirFile = new File(baseOutputDir + File.separator + "ADC");
//                    
//                    // clean-up prev ADC dir
//                    if (foundOutputDirADC && !mergeADC) {
//                        File backupDir = new File(adcDirFile.getAbsolutePath() + "_old_" + System.currentTimeMillis());
//                        boolean success = adcDirFile.renameTo(backupDir);
//                        if (success) {
//                            log("Failed to move previous ADC data to : " + backupDir);
//                        }
//                    }
//                    
//                    for (File file : adcFiles) {
//                        try {
//                            renameTo(association, file, new File(adcDirFile.getAbsolutePath() + File.separator + file.getName()));
//                        } catch (IOException e) {
//                            e.printStackTrace();
//                        }
//                    }
//                }
//                
//                if (foundDWI && !usePrevDWI) {
//                    log("Found DWI volume in completed transfer. Moving to " + baseOutputDir);
//                    
//                    File dwiDirFile = new File(baseOutputDir + File.separator + "DWI");
//                    
//                    // clean-up prev DWI dir
//                    if (foundOutputDirDWI && !mergeDWI) {
//                        File backupDir = new File(dwiDirFile.getAbsolutePath() + "_old_" + System.currentTimeMillis());
//                        boolean success = dwiDirFile.renameTo(backupDir);
//                        if (success) {
//                            log("Failed to move previous DWI data to : " + backupDir);
//                        }
//                    }
//                    
//                    for (File file : dwiFiles) {
//                        try {
//                            renameTo(association, file, new File(dwiDirFile.getAbsolutePath() + File.separator + file.getName()));
//                        } catch (IOException e) {
//                            e.printStackTrace();
//                        }
//                    }
//                }
//                
//                if (foundPWI && !usePrevPWI) {
//                    log("Found PWI volume in completed transfer. Moving to " + baseOutputDir);
//                    
//                    File pwiDirFile = new File(baseOutputDir + File.separator + "PWI");
//                    
//                    // clean-up prev PWI dir
//                    if (foundOutputDirPWI && !mergePWI) {
//                        File backupDir = new File(pwiDirFile.getAbsolutePath() + "_old_" + System.currentTimeMillis());
//                        boolean success = pwiDirFile.renameTo(backupDir);
//                        if (success) {
//                            log("Failed to move previous PWI data to : " + backupDir);
//                        }
//                    }
//                    
//                    for (File file : pwiFiles) {
//                        try {
//                            renameTo(association, file, new File(pwiDirFile.getAbsolutePath() + File.separator + file.getName()));
//                        } catch (IOException e) {
//                            e.printStackTrace();
//                        }
//                    }
//                }
//                
//                
//                
//                if (!alreadySegmented) {
//                	if (foundPWI || usePrevPWI) {
//                		// use PWI version of the coretool algo
//	                	if (((foundADC && foundDWI) || (foundADC && usePrevDWI) || (usePrevADC && foundDWI))) {
//		                    // check that the number of files in the ADC and DWI directories are the same
//		                    File adcDirFile = new File(baseOutputDir + File.separator + "ADC");
//		                    int numAdcFiles = adcDirFile.listFiles().length;
//		                    File dwiDirFile = new File(baseOutputDir + File.separator + "DWI");
//		                    int numDwiFiles = dwiDirFile.listFiles().length;
//		                    
//		                    File pwiDirFile = new File(baseOutputDir + File.separator + "PWI");
//		                    int numPwiFiles = pwiDirFile.listFiles().length;
//		                    
//		                    if (numAdcFiles == numDwiFiles && numAdcFiles >= minExpectedSlices) {
//		                    	if (numPwiFiles >= minExpectedSlicesPWI) {
//		                    		log("Received " + numPwiFiles + " PWI files, meeting minimum threshold (" + minExpectedSlicesPWI + ")");
//		                    		log("Running PWI-incorporating segmentation on datasets in " + baseOutputDir.getAbsolutePath());
//		                    		new PlugInDialogStrokeSegmentationPWI(StrokeSegmentationDicomReceiverPWI.this, baseOutputDir.getAbsolutePath(), false);
//		                    	} else {
//		                    		log("Received " + numPwiFiles + " PWI files, NOT meeting minimum threshold (" + minExpectedSlicesPWI + ")");
//		                    		log("Running non-PWI segmentation on datasets in " + baseOutputDir.getAbsolutePath());
//		                    		new PlugInDialogStrokeSegmentationPWI(StrokeSegmentationDicomReceiverPWI.this, baseOutputDir.getAbsolutePath(), true);
//		                    	}
//		                    } else {
//		                        log("Expected number of DWI or ADC files not reached " + minExpectedSlices + ". Skipping segmentation. " + baseOutputDir + " --- ADC: " + numAdcFiles + " --- DWI: " + numDwiFiles);
//		                    }
//		                } else {
//		                	log("DICOM transfer complete - no segmentation performed (new ADC: " + foundADC + " -- new DWI: " + foundDWI + " -- new PWI: " + foundPWI + " -- old ADC: " + usePrevADC + " -- old DWI: " + usePrevDWI + " -- old PWI: " + usePrevPWI + ").");
//		                }
//                	} else {
//                		if (((foundADC && foundDWI) || (foundADC && usePrevDWI) || (usePrevADC && foundDWI))) {
//		                    // check that the number of files in the ADC and DWI directories are the same
//		                    File adcDirFile = new File(baseOutputDir + File.separator + "ADC");
//		                    int numAdcFiles = adcDirFile.listFiles().length;
//		                    File dwiDirFile = new File(baseOutputDir + File.separator + "DWI");
//		                    int numDwiFiles = dwiDirFile.listFiles().length;
//		                    
//		                    if (numAdcFiles == numDwiFiles && numAdcFiles >= minExpectedSlices) {
//	                    		log("Running non-PWI segmentation on datasets in " + baseOutputDir.getAbsolutePath());
//	                    		new PlugInDialogStrokeSegmentationPWI(StrokeSegmentationDicomReceiverPWI.this, baseOutputDir.getAbsolutePath(), true);
//		                    } else {
//		                        log("Expected number of DWI or ADC files not reached " + minExpectedSlices + ". Skipping segmentation. " + baseOutputDir + " --- ADC: " + numAdcFiles + " --- DWI: " + numDwiFiles);
//		                    }
//		                } else {
//		                	log("DICOM transfer complete - no segmentation performed (new ADC: " + foundADC + " -- new DWI: " + foundDWI + " -- new PWI: " + foundPWI + " -- old ADC: " + usePrevADC + " -- old DWI: " + usePrevDWI + " -- old PWI: " + usePrevPWI + ").");
//		                }
//                	}
//                } else {
//                	log("DICOM transfer complete - no segmentation performed (new ADC: " + foundADC + " -- new DWI: " + foundDWI + " -- new PWI: " + foundPWI + " -- old ADC: " + usePrevADC + " -- old DWI: " + usePrevDWI + " -- old PWI: " + usePrevPWI + ").");
//                }
                
                // check if incoming dicom dir is empty and remove if it is
                try {
                    File incomingDir = new File(storageDir + File.separator + "incoming" + File.separator + studyDate);
                    
                    if (isDirectoryEmpty(incomingDir)) {
                        FileUtils.deleteDirectory(incomingDir);
                    } else {
                        File[] incomingSubDirs = incomingDir.listFiles();
                        for (File file : incomingSubDirs) {
                            if (file.isDirectory()) {
                                if (isDirectoryEmpty(file)) {
                                    FileUtils.deleteDirectory(file);
                                } else {
                                    File[] newList = file.listFiles();
                                    for (File newFile : newList) {
                                        if (newFile.isDirectory()) {
                                            if (isDirectoryEmpty(newFile)) {
                                                FileUtils.deleteDirectory(newFile);
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                } catch (IOException e) {
                    e.printStackTrace();
                }
                
                addedCloseListener = false;
                receivedFileList.removeAllElements();
                receivedFileList = null;
            } finally {
                fileListLock.unlock();
            }
        }
    }
    
    public boolean shutdownReceiver() {
        try {
            device.waitForNoOpenConnections();
            
            device.unbindConnections();
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
        
        return true;
    }
    
    /**
     * Append a line to the log output area in the Log tab.
     * 
     * @param line The line to append (do not include the trailing newline).
     */
    public void log(final String line) {
        logOutputArea.getTextArea().append(line + "\n");
        System.out.println("*****\t" + line);
    }
    
    public void emailReport(ModelImage adcImage, Vector<File> lightboxFileList, Hashtable<File, Double> coreObjectTable, Hashtable<Integer, Hashtable<File, Double>> tmaxTable, double resFactorCC) {
        // generate report for website
        String reportTxt = generateReport(adcImage, lightboxFileList, coreObjectTable, tmaxTable, resFactorCC, false);
        
        if (reportTxt == null) {
        	return;
        }
        
        File firstLightboxFile = lightboxFileList.get(0);
        
        String outputDir = firstLightboxFile.getParent();
        final String htmlReportPath = outputDir + File.separator + "core_seg_report.html";
        
        PrintWriter out;
        try {
            String fileTxt = reportTxt;
            
            for (int i = 0; i < lightboxFileList.size(); i++) {
                fileTxt = fileTxt.replaceAll(reportCidBase + (i + 1), lightboxFileList.get(i).getName());
            }
            
            out = new PrintWriter(htmlReportPath);
            out.println("<html>");
            out.print(fileTxt);
            out.println("</html>");
            out.close();
            out = null;
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        	MipavUtil.displayError("Unable to write core segmentation report: " + e.getMessage());
        }
        
        // email report has different metadata
        String emailReportTxt = generateReport(adcImage, lightboxFileList, coreObjectTable, tmaxTable, resFactorCC, true);
        
        final String emailReportPath = outputDir + File.separator + "core_seg_report_email.html";
        
        
        try {
            String fileTxt = emailReportTxt;
            
            for (int i = 0; i < lightboxFileList.size(); i++) {
                fileTxt = fileTxt.replaceAll(reportCidBase + (i + 1), lightboxFileList.get(i).getName());
            }
            
            out = new PrintWriter(emailReportPath);
            out.println("<html>");
            out.print(fileTxt);
            out.println("</html>");
            out.close();
            out = null;
        } catch (FileNotFoundException e) {
            e.printStackTrace();
            MipavUtil.displayError("Unable to write core segmentation email report: " + e.getMessage());
        }
        
        if (reportDir != null) {
            File reportDirFile = new File(reportDir);
            if (!reportDirFile.exists()) {
                reportDirFile.mkdirs();
            }
            
            try {
                File reportSubDir = new File(reportDir + File.separator + firstLightboxFile.getParentFile().getParentFile().getName() + File.separator + firstLightboxFile.getParentFile().getName());
                
                // check to make sure the report/output dirs aren't the same before moving the report
                if (!outputDir.equals(reportSubDir.getAbsolutePath())) {
                    reportSubDir.mkdirs();
                    
                    for (File file : lightboxFileList) {
                        FileUtils.copyFileToDirectory(file, reportSubDir);
                    }
                    
                    FileUtils.copyFileToDirectory(new File(htmlReportPath), reportSubDir);
                    FileUtils.copyFileToDirectory(new File(emailReportPath), reportSubDir);
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        
        if (doEmailReport && readEmailConfig()) {
            Properties props = new Properties();
    		props.put("mail.smtp.auth", "true");
    		props.put("mail.smtp.starttls.enable", "true");
    		props.put("mail.smtp.host", emailHost);
    		props.put("mail.smtp.port", emailPort);
    		props.put("mail.smtp.socketFactory.port", emailPort);
    		props.put("mail.smtp.socketFactory.class", "javax.net.ssl.SSLSocketFactory");
    
    		Session session = Session.getInstance(props, new javax.mail.Authenticator() {
    			protected PasswordAuthentication getPasswordAuthentication() {
    				return new PasswordAuthentication(emailUsername, emailPassword);
    			}
    		});
    
    		try {
    			MimeMultipart multipartContent = new MimeMultipart("related");
    			
    			BodyPart messageBody = new MimeBodyPart();
    			messageBody.setContent(reportTxt, "text/html");
    			multipartContent.addBodyPart(messageBody);
    			
    			for (int i = 0; i < lightboxFileList.size(); i++) {
    			    messageBody = new MimeBodyPart();
                    DataSource imgDS = new FileDataSource(lightboxFileList.get(i));
                    messageBody.setDataHandler(new DataHandler(imgDS));
                    messageBody.setHeader("Content-ID", "<" + reportCidImgName + (i + 1) + ">");
                    multipartContent.addBodyPart(messageBody);
    			}
    			
    			Message message = new MimeMessage(session);
    
    			// Set From: header field of the header.
    			message.setFrom(new InternetAddress(emailFrom));
    
    			// Set To: header field of the header.
    			message.setRecipients(Message.RecipientType.TO, InternetAddress.parse(emailTo));
    
    			// Set Subject: header field
    			message.setSubject("MIPAV Stroke Core Segmentation Report");
    
    			message.setContent(multipartContent);
    
    			// Send message
    			Transport.send(message);
    
    			log("Segmentation report successfully emailed.");
    		} catch (MessagingException e) {
    			e.printStackTrace();
    		}
        }
    }
    
    private String generateReport(ModelImage adcImage, Vector<File> lightboxFileList, Hashtable<File, Double> coreObjectTable, Hashtable<Integer, Hashtable<File, Double>> tmaxTable, double resFactorCC, boolean isEmailSafeReport) {
        final DecimalFormat format = new DecimalFormat("#######.#");
        
        FileInfoDicom fileInfoDicom = (FileInfoDicom) adcImage.getFileInfo(0);
        
        String curDateTimeStr = new SimpleDateFormat("yyyy-MM-dd'T'HH.mm.ss").format(new Date());
        
        String studyDateStr = (String) fileInfoDicom.getTagTable().getValue("0008,0020");
        String studyTimeStr = (String) fileInfoDicom.getTagTable().getValue("0008,0030");
        String patientName = (String) fileInfoDicom.getTagTable().getValue("0010,0010");
        
        String institutionStr = (String) fileInfoDicom.getTagTable().getValue("0008,0080");
        String ageStr = parseAge((String) fileInfoDicom.getTagTable().getValue("0010,1010"));
        String sexStr = (String) fileInfoDicom.getTagTable().getValue("0010,0040");
        
        //String reportTxt = "<html>\n";
        String reportTxt = "";
        reportTxt += "<h1>" + "MIPAV Stroke Core Segmentation Report" + "</h1>\n";
        reportTxt += "<ul>\n";
        if (!isEmailSafeReport) {
            reportTxt += "<li>" + "<b>" + "Time of segmentation run: " + "</b>" + curDateTimeStr + "</li>\n";
            reportTxt += "<li>" + "<b>" + "Study date and time: " + "</b>" + convertDateTimeToISOFormat(studyDateStr, studyTimeStr) + "</li>\n";
            reportTxt += "<li>" + "<b>" + "Patient last name initial: " + "</b>" + getInitialFromName(patientName) + "</li>\n";
        } else {
            reportTxt += "<li>" + "<b>" + "Time of segmentation run: " + "</b>" + curDateTimeStr + "</li>\n";
            reportTxt += "<li>" + "<b>" + "Institution Name: " + "</b>" + institutionStr + "</li>";
            reportTxt += "<li>" + "<b>" + "Patient Age: " + "</b>" + ageStr + "</li>";
            reportTxt += "<li>" + "<b>" + "Patient Sex: " + "</b>" + sexStr + "</li>";
        }
        //reportTxt += "<li>" + "<b>" + "" + "</b>" + "" + "</li>";
        reportTxt += "</ul>\n";
        
        for (int i = 0; i < lightboxFileList.size(); i++) {
            int passNum = i + 1;
            
            
            String passDescr = "";
            String passDetails = "";
            if (lightboxFileList.size() == 2) {
                // core no PWI, DWI
                
                if (passNum == 1) {
                    passDetails = "";
                    passDescr = "ADC image with core segmentation" + passDetails;
                } else if (passNum == 2) {
                    passDescr = "DWI image";
                }
                
                reportTxt += "<h3>" + passDescr + "</h3>\n";
                
                if (coreObjectTable.get(lightboxFileList.get(i)) > 0) {
                    String coreSegVol = format.format(coreObjectTable.get(lightboxFileList.get(i)).doubleValue() * resFactorCC);
                    reportTxt += "<p>" + "<b>" + "Core segmentation volume (mL): " + "</b>" + coreSegVol + "</p>\n";
                } else if (passNum == 1) {
                    reportTxt += "<p>" + "<b>" + "No core region found.</p>\n";
                }
            } else if (lightboxFileList.size() == 6) {
                // core no PWI, perfusion, core w/ PWI, dwi
                
                if (passNum == 1) {
                    passDetails = "";
                    passDescr = "ADC image with core segmentation" + passDetails;
                } else if (passNum == 2) {
                    passDescr = "PWI Tmax > 6s";
                } else if (passNum == 4) {
                    passDetails = " -- considering Tmax lesion";
                    passDescr = "ADC image with core segmentation" + passDetails;
                } else if (passNum == 3) {
                    passDescr = "DWI image";
                } else if (passNum == 5) {
                    passDescr = "AIF curve";
                } else if (passNum == 6) {
                    passDescr = "AIF location";
                }
                
                reportTxt += "<h3>" + passDescr + "</h3>\n";
                
                if (coreObjectTable.get(lightboxFileList.get(i)) > 0) {
                    String coreSegVol = format.format(coreObjectTable.get(lightboxFileList.get(i)).doubleValue() * resFactorCC);
                    reportTxt += "<p>" + "<b>" + "Core segmentation volume (mL): " + "</b>" + coreSegVol + "</p>\n";
                    
                    if (passNum == 1) {
                        if (tmaxTable.get(6).get(lightboxFileList.get(1)) > 0) {
                            double perfVol = tmaxTable.get(6).get(lightboxFileList.get(1)).doubleValue() + tmaxTable.get(8).get(lightboxFileList.get(1)).doubleValue() + tmaxTable.get(10).get(lightboxFileList.get(1)).doubleValue();
                            
                            double noPwiCore = coreObjectTable.get(lightboxFileList.get(0)).doubleValue();
                            double noPwiCoreDiff = (perfVol - noPwiCore) * resFactorCC;
                            double noPwiCoreRatio = perfVol / noPwiCore;
                        
                            reportTxt += "<p>" + "<b>" + "Mismatch volume (mL): " + "</b>" + format.format(noPwiCoreDiff) + "</p>\n";
                            reportTxt += "<p>" + "<b>" + "Mismatch ratio: " + "</b>" + format.format(noPwiCoreRatio) + "</p>\n";
                        }
                    } else if (passNum == 4) {
                        if (tmaxTable.get(6).get(lightboxFileList.get(1)) > 0) {
                            double perfVol = tmaxTable.get(6).get(lightboxFileList.get(1)).doubleValue() + tmaxTable.get(8).get(lightboxFileList.get(1)).doubleValue() + tmaxTable.get(10).get(lightboxFileList.get(1)).doubleValue();
                            
                            double pwiCore = coreObjectTable.get(lightboxFileList.get(3)).doubleValue();
                            double pwiCoreDiff = (perfVol - pwiCore) * resFactorCC;
                            double pwiCoreRatio = perfVol / pwiCore;
                        
                            reportTxt += "<p>" + "<b>" + "Mismatch volume (mL): " + "</b>" + format.format(pwiCoreDiff) + "</p>\n";
                            reportTxt += "<p>" + "<b>" + "Mismatch ratio: " + "</b>" + format.format(pwiCoreRatio) + "</p>\n";
                        }
                    }
                } else if (tmaxTable.get(6).get(lightboxFileList.get(i)) > 0) {
                    double[] perfVolList = new double[4];
                    perfVolList[3] = tmaxTable.get(10).get(lightboxFileList.get(i)).doubleValue();
                    perfVolList[2] = perfVolList[3] + tmaxTable.get(8).get(lightboxFileList.get(i)).doubleValue();
                    perfVolList[1] = perfVolList[2] + tmaxTable.get(6).get(lightboxFileList.get(i)).doubleValue();
                    perfVolList[0] = perfVolList[1] + tmaxTable.get(4).get(lightboxFileList.get(i)).doubleValue();
                    
                    reportTxt += "<p>" + "<b>" + "Tmax > 4 volume (mL): " + "</b>" + format.format(perfVolList[0] * resFactorCC) + "</p>\n";
                    reportTxt += "<p>" + "<b>" + "Tmax > 6 volume (mL): " + "</b>" + format.format(perfVolList[1] * resFactorCC) + "</p>\n";
                    reportTxt += "<p>" + "<b>" + "Tmax > 8 volume (mL): " + "</b>" + format.format(perfVolList[2] * resFactorCC) + "</p>\n";
                    reportTxt += "<p>" + "<b>" + "Tmax > 10 volume (mL): " + "</b>" + format.format(perfVolList[3] * resFactorCC) + "</p>\n";
                    reportTxt += "<p>" + "<b>" + "Hypoperfusion Index (Tmax > 10s / Tmax > 6s): " + "</b>" + format.format(perfVolList[3] / perfVolList[1]) + "</p>\n";
                } else if (passNum == 1 || passNum == 4) {
                    reportTxt += "<p>" + "<b>" + "No core region found.</p>\n";
                } else if (passNum == 2) {
                    reportTxt += "<p>" + "<b>" + "No perfusion region found.</p>\n";
                }
            }
            
            //reportTxt += "<a href='" + dwiPdfImage + "'><img src='" + dwiPdfImage + "' alt='ADC volume with core segmentation' width='" + imgDisplay + "'/></a>\n";
            reportTxt += "<img src='" + reportCidBase + passNum + "' alt='" + passDescr + "'/>\n";
        }
        
//        reportTxt += "<h3>" + "ADC image with thresholded regions prior to core volume calculation" + "</h3>\n";
//        //reportTxt += "<a href='" + adcPdfImage + "'><img src='" + adcPdfImage + "' alt='ADC volume with thresholded regions' width='" + imgDisplay + "'/></a>\n";
//        reportTxt += "<img src='" + reportThreshCid + "' alt='ADC image with thresholded regions prior to core volume calculation'/>\n";
        
        //reportTxt += "</html>\n";
        
        return reportTxt;
    }
    
    /**
     * Converts from DICOM/US date and time format (MM/DD/YYYY) or M/D/YYYY to ISO 8601 format (YYYY-MM-DDThh:mm:ss).
     * 
     * @param date A date string in the format MM/DD/YYYY or M/D/YYYY.
     * @param time A time string in the format hh:mm:ss.fract or hhmmss.fract.
     * @return An ISO 8601 formatted version of the given date and time (or the original string if not in the DICOM/US
     *         date format).
     */
    private static final String convertDateTimeToISOFormat(final String date, String time) {
        if (date == null) {
            return "";
        }
        if (time == null) {
            time = "";
        }

        String isoDate = date.trim();
        String isoTime = time.trim();

        final String datePattern = "^(\\d{1,2})[/-]*(\\d{1,2})[/-]*(\\d{4})$";
        final String timePattern = "^(\\d{2})[:]?(\\d{2})[:]?(\\d{2})[.]?(\\d*)$";

        Pattern p = Pattern.compile(datePattern);
        Matcher m = p.matcher(isoDate);
        if (m.find()) {
            String month = m.group(1);
            String day = m.group(2);
            final String year = m.group(3);
            // add leading zeroes, if necessary
            if (month.length() == 1) {
                month = "0" + month;
            }
            if (day.length() == 1) {
                day = "0" + day;
            }
            isoDate = year + "-" + month + "-" + day;
        }

        p = Pattern.compile(timePattern);
        m = p.matcher(isoTime);
        if (m.find()) {
            String hour = m.group(1);
            String min = m.group(2);
            String sec = m.group(3);
            final String frac = m.group(4);
            // add leading zeroes, if necessary
            if (hour.length() == 1) {
                hour = "0" + hour;
            }
            if (min.length() == 1) {
                min = "0" + min;
            }
            if (sec.length() == 1) {
                sec = "0" + sec;
            }
            isoTime = hour + ":" + min + ":" + sec;
            if (frac.length() > 0) {
                isoTime += "." + frac;
            }
        }

        if (isoTime.equals("")) {
            return isoDate;
        } else {
            return isoDate + "T" + isoTime;
        }
    }

    private String getInitialFromName(final String dicomName) {
        return dicomName.substring(0, 1);
    }
    
    private boolean isRegisteredVol(final Attributes dicomAttr) {
        final String series = dicomAttr.getString(TagUtils.toTag(0x0008, 0x103E));
        final String protocol = dicomAttr.getString(TagUtils.toTag(0x0018, 0x1030));
        
        return PlugInDialogStrokeSegmentationPWI.isRegisteredVol(series, protocol);
    }
    
    private boolean isDirectoryEmpty(final File dir) {
        if (dir.isFile()) {
            return false;
        }
        
        File[] list = dir.listFiles();
        if (list.length == 0) {
            return true;
        } else {
            for (File file : list) {
                if (file.isDirectory()) {
                    boolean subdirEmpty = isDirectoryEmpty(file);
                    if (!subdirEmpty) {
                        return false;
                    }
                } else {
                    return false;
                }
            }
        }
        
        return true;
    }
    
    private boolean readEmailConfig() {
        try {
            final InputStream in = getClass().getResourceAsStream(configFileName);
            if (in != null) {
                final Properties prop = new Properties();
                try {
                    prop.load(in);
                } catch (final IOException e) {
                    Preferences.debug("Unable to load stroke segementation listener plugin preferences file: " + configFileName + "\n", Preferences.DEBUG_MINOR);
                    e.printStackTrace();
                    if (in != null) {
                        in.close();
                    }
                    return false;
                }
                
                emailFrom = prop.getProperty("emailFrom");
                
                emailTo = prop.getProperty("emailTo");
                
                emailUsername = prop.getProperty("emailUsername");
                
                emailPassword = prop.getProperty("emailPassword");
                
                emailHost = prop.getProperty("emailHost");

                emailPort = prop.getProperty("emailPort");

                String format = prop.getProperty("listenerInitStorageFormat");
                if (format != null && !format.equals("")) {
                    storageFilePathFormat = format;
                }
                
                if (in != null) {
                    in.close();
                }
                return true;
            } else {
                // couldn't load file
                return false;
            }
        } catch (IOException e) {
            e.printStackTrace();
            log("Error loading listener properties file: " + configFileName);
            return false;
        }
    }
    
    private static final String parseAge(final String dicomAge) {
        if (dicomAge == null || dicomAge.equals("")) {
            return "-- No Age Found --";
        }
        
        final String num = dicomAge.substring(0, dicomAge.length() - 1);
        final String type = dicomAge.substring(dicomAge.length() - 1, dicomAge.length());

        if (type.equalsIgnoreCase("Y") && Integer.parseInt(num) > 90) {
            return "90Y";
        }

        return dicomAge;
    }
    
    private class CoreToolProcessingThread implements Runnable {
    	public boolean done = false;
    	
    	public long timeStarted;
    	public long timeLimitMilli;
    	public String receivedFilesPath;
    	
    	public int[] minPwiSlices;
    	public int minAdcDwiSlices;
    	
    	public int curPwiSlices = 0;
    	public int curAdcSlices = 0;
    	public int curDwiSlices = 0;
    	
    	public CoreToolProcessingThread(String recvPath, int maxMinutes, int[] minPwi, int minAdc) {
    		timeStarted = System.currentTimeMillis();
    		receivedFilesPath = recvPath;
    		
    		timeLimitMilli = maxMinutes * 60 * 1000;
    		minPwiSlices = minPwi;
    		minAdcDwiSlices = minAdc;
    	}
    	
		@Override
		public synchronized void run() {
			while (!done) {
				long curTimeSinceStart = getTimeSinceStart();
				if (curTimeSinceStart < timeLimitMilli) {
					// check num slices received. if not enough pwi, just run with adc/dwi.  if enough, run adc/dwi/pwi
				    boolean isAllowedPwiSliceNum = false;
				    for (int allowedSlices : minPwiSlices) {
				        if (curPwiSlices == allowedSlices) {
				            isAllowedPwiSliceNum = true;
    						break;
    					}
				    }
				    
				    if (isAllowedPwiSliceNum) {
    				    if (curAdcSlices >= minAdcDwiSlices && curAdcSlices == curDwiSlices) {
                            // run with PWI/ADC/DWI
                            done = true;
                            log("Received " + curPwiSlices + " PWI files, meeting threshold (" + PlugInDialogStrokeSegmentationListenerPWI.intArrayToStr(minExpectedSlicesPWI) + ")");
                            log("Running PWI-incorporating segmentation on datasets in " + receivedFilesPath);
                            
                            // remove listener thread since we are starting the algorithm
                            coreToolThreadTable.remove(receivedFilesPath);
                            
                            new PlugInDialogStrokeSegmentationPWI(StrokeSegmentationDicomReceiverPWI.this, receivedFilesPath, false);
                        }
				    }
					
					// otherwise, loop around and do nothing wait again until more data arrives or the time limit is hit
					log("DICOM transfer complete - no segmentation performed. " + receivedFilesPath + " --- ADC: " + curAdcSlices + " --- DWI: " + curDwiSlices + " --- PWI: " + curPwiSlices);
					
	                // wait for a while or until a new file transfer is received
					log("Waiting for " + (timeLimitMilli - curTimeSinceStart) / 1000 + " seconds");
                    try {
                        wait(timeLimitMilli - curTimeSinceStart);
                    } catch (InterruptedException e) {
                        Thread.currentThread().interrupt();
                        e.printStackTrace();
                    }
				} else {
					// hit time limit. check num slices received. if not enough pwi, just run with adc/dwi.  if enough, run adc/dwi/pwi
					log("Reached transfer time limit for " + receivedFilesPath);
					done = true;
					boolean isAllowedPwiSliceNum = false;
                    for (int allowedSlices : minPwiSlices) {
                        if (curPwiSlices == allowedSlices) {
                            isAllowedPwiSliceNum = true;
                            break;
                        }
                    }
                    
                    if (isAllowedPwiSliceNum) {
                        if (curAdcSlices >= minAdcDwiSlices && curAdcSlices == curDwiSlices) {
                            // run with PWI/ADC/DWI
                            log("Received " + curPwiSlices + " PWI files, meeting threshold (" + PlugInDialogStrokeSegmentationListenerPWI.intArrayToStr(minExpectedSlicesPWI) + ")");
                            log("Running PWI-incorporating segmentation on datasets in " + receivedFilesPath);
                            
                            // remove listener thread since we are starting the algorithm
                            coreToolThreadTable.remove(receivedFilesPath);
                            
                            new PlugInDialogStrokeSegmentationPWI(StrokeSegmentationDicomReceiverPWI.this, receivedFilesPath, false);
                        } else {
                            log("Expected number of DWI or ADC files not reached " + minExpectedSlices + ". Skipping segmentation. " + receivedFilesPath + " --- ADC: " + curAdcSlices + " --- DWI: " + curDwiSlices + " --- PWI: " + curPwiSlices);
                        }
					} else {
						if (curAdcSlices >= minAdcDwiSlices && curAdcSlices == curDwiSlices) {
							// run with just ADC/DWI
							log("Received " + curPwiSlices + " PWI files, NOT meeting threshold (" + PlugInDialogStrokeSegmentationListenerPWI.intArrayToStr(minExpectedSlicesPWI) + ")");
                    		log("Running non-PWI segmentation on datasets in " + receivedFilesPath);
                    		
                    		// remove listener thread since we are starting the algorithm
                            coreToolThreadTable.remove(receivedFilesPath);
                    		
                    		new PlugInDialogStrokeSegmentationPWI(StrokeSegmentationDicomReceiverPWI.this, receivedFilesPath, true);
						} else {
							log("Expected number of DWI or ADC files not reached " + minExpectedSlices + ". Skipping segmentation. " + receivedFilesPath + " --- ADC: " + curAdcSlices + " --- DWI: " + curDwiSlices + " --- PWI: " + curPwiSlices);
						}
					}
				}
			}
		}
		
		public long getTimeSinceStart() {
			return System.currentTimeMillis() - timeStarted;
		}
		
		public synchronized void addToPwiSliceCount(int numSlices) {
			curPwiSlices += numSlices;
			notifyAll();
		}
		
		public synchronized void resetPwiSliceCount() {
            curPwiSlices = 0;
            notifyAll();
        }
		
		public synchronized void addToAdcSliceCount(int numSlices) {
            curAdcSlices += numSlices;
            notifyAll();
        }
		
		public synchronized void resetAdcSliceCount() {
            curAdcSlices = 0;
            notifyAll();
        }
		
		public synchronized void addToDwiSliceCount(int numSlices) {
			curDwiSlices += numSlices;
			notifyAll();
		}
		
		public synchronized void resetDwiSliceCount() {
            curDwiSlices = 0;
            notifyAll();
        }
    }
}