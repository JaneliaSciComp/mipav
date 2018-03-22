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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.activation.*;
import javax.mail.*;
import javax.mail.internet.*;

import org.dcm4che3.data.Tag;
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
public class StrokeSegmentationDicomReceiver {

    private static final Logger LOG = LoggerFactory.getLogger(StrokeSegmentationDicomReceiver.class);

    private static ResourceBundle rb =
        ResourceBundle.getBundle("org.dcm4che3.tool.storescp.messages");
    private static final String PART_EXT = ".part";

    private final Device device = new Device("storescp");
    private final ApplicationEntity ae = new ApplicationEntity("*");
    private final Connection conn = new Connection();
    private File storageDir;
    private AttributesFormat filePathFormat;
    private int status;
    private final BasicCStoreSCP cstoreSCP = new BasicCStoreSCP("*") {

        @Override
        protected void store(Association as, PresentationContext pc,
                Attributes rq, PDVInputStream data, Attributes rsp)
                throws IOException {
            rsp.setInt(Tag.Status, VR.US, status);
            if (storageDir == null)
                return;
            
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
        }

    };
    
    private boolean addedCloseListener = false;
    
    private Vector<File> receivedFileList;
    
    private String serverIP;
    private int serverPort;
    private String serverAE;
    
    private boolean doEmailReport;
    
    private static final String reportDwiCid = "cid:dwi-image";
    private static final String reportAdcCid = "cid:adc-image";
    
    private WidgetFactory.ScrollTextArea logOutputArea;
    
    private static final String configFileName = "stroke_seg_listener.properties";
    
    String emailFrom;
    String emailTo;
    String emailUsername;
    String emailPassword;
    String emailHost;
    String emailPort;

    public StrokeSegmentationDicomReceiver(final String ip, final int port, final String curAE, final String outputDir, final boolean doEmail, final WidgetFactory.ScrollTextArea area) throws IOException {
        serverIP = ip;
        serverPort = port;
        serverAE = curAE;
        
        doEmailReport = doEmail;
        
        logOutputArea = area;
        
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
        
        setStorageFilePathFormat("{00080020}.{00080030}/{00200011}.{00200012}.{00200013}.dcm");
        
        // anon versions don't include study/series uids
        //setStorageFilePathFormat("{00100020}/{0020000D}/{0020000E}/{00080008}/{00080018}.dcm");
        
        ExecutorService executorService = Executors.newCachedThreadPool();
        ScheduledExecutorService scheduledExecutorService = Executors.newSingleThreadScheduledExecutor();
        device.setScheduledExecutor(scheduledExecutorService);
        device.setExecutor(executorService);
        try {
            device.bindConnections();
        } catch (GeneralSecurityException e) {
            // TODO Auto-generated catch block
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
            log("Received association close request");
            
            boolean foundADC = false;
            boolean foundDWI = false;
            
            Vector<File> adcFiles = new Vector<File>();
            Vector<File> dwiFiles = new Vector<File>();
            
            for (File file : receivedFileList) {
                try {
                    Attributes attr = parse(file);
                    
                    final String[] imageTypes = attr.getStrings(TagUtils.toTag(0x0008, 0x0008));
                    
                    for (String val : imageTypes) {
                        if (val.equalsIgnoreCase("ADC") || val.equalsIgnoreCase("ADC_UNSPECIFIED")) {
                            adcFiles.add(file);
                            foundADC = true;
                            break;
                        } else if (val.equalsIgnoreCase("SE") || val.equalsIgnoreCase("M_SE") || val.equalsIgnoreCase("TRACEW")) {
                            dwiFiles.add(file);
                            foundDWI = true;
                            break;
                        }
                    }
                } catch (IOException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
            
            String baseDicomDir = null;
            
            // move ADC and DWI files to their own dir under parent inside outputDir
            if (foundADC) {
                log("Found ADC volume in completed transfer.");
                
                for (File file : adcFiles) {
                    try {
                        if (baseDicomDir == null) {
                            baseDicomDir = file.getParent();
                        }
                        
                        renameTo(association, file, new File(file.getParent() + File.separator + "ADC" + File.separator + file.getName()));
                    } catch (IOException e) {
                        // TODO Auto-generated catch block
                        e.printStackTrace();
                    }
                }
            }
            
            if (foundDWI) {
                log("Found DWI volume in completed transfer.");
                
                for (File file : dwiFiles) {
                    try {
                        if (baseDicomDir == null) {
                            baseDicomDir = file.getParent();
                        }
                        
                        renameTo(association, file, new File(file.getParent() + File.separator + "DWI" + File.separator + file.getName()));
                    } catch (IOException e) {
                        // TODO Auto-generated catch block
                        e.printStackTrace();
                    }
                }
            }
            
            if (foundADC && foundDWI) {
                log("Running segmentation on datasets in " + baseDicomDir);
                new PlugInDialogStrokeSegmentation(StrokeSegmentationDicomReceiver.this, baseDicomDir);
            }
            
            addedCloseListener = false;
            receivedFileList.removeAllElements();
            receivedFileList = null;
        }
    }
    
    public boolean shutdownReceiver() {
        try {
            device.waitForNoOpenConnections();
            
            device.unbindConnections();
        } catch (Exception e) {
            // TODO Auto-generated catch block
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
    }
    
    public void emailReport(ModelImage adcImage, File adcLightboxFile, File dwiLightboxFile, double coreVolCC) {
        String reportTxt = generateReport(adcImage, adcLightboxFile, dwiLightboxFile, coreVolCC);
        
        if (reportTxt == null) {
        	return;
        }
        
        String outputDir = adcLightboxFile.getParent();
        final String htmlReportPath = outputDir + File.separator + "core_seg_report.html";
        
        PrintWriter out;
        try {
            String fileTxt = reportTxt.replaceAll(reportDwiCid, dwiLightboxFile.getName());
            fileTxt = fileTxt.replaceAll(reportAdcCid, adcLightboxFile.getName());
            
            out = new PrintWriter(htmlReportPath);
            out.println("<html>");
            out.print(fileTxt);
            out.println("</html>");
            out.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        	MipavUtil.displayError("Unable to write core segmentation report: " + e.getMessage());
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
    			
    			messageBody = new MimeBodyPart();
    			DataSource imgDS = new FileDataSource(adcLightboxFile);
    			messageBody.setDataHandler(new DataHandler(imgDS));
    			messageBody.setHeader("Content-ID", "<adc-image>");
    			multipartContent.addBodyPart(messageBody);
    			
    			messageBody = new MimeBodyPart();
    			imgDS = new FileDataSource(dwiLightboxFile);
    			messageBody.setDataHandler(new DataHandler(imgDS));
    			messageBody.setHeader("Content-ID", "<dwi-image>");
    			multipartContent.addBodyPart(messageBody);
    			
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
    
    private String generateReport(ModelImage adcImage, File adcLightboxFile, File dwiLightboxFile, double coreVolCC) {
        final DecimalFormat format = new DecimalFormat("#######.####");
        
        FileInfoDicom fileInfoDicom = (FileInfoDicom) adcImage.getFileInfo(0);
        
        String curDateTimeStr = new SimpleDateFormat("yyyy-MM-dd'T'HH.mm.ss").format(new Date());
        
        String studyDateStr = (String) fileInfoDicom.getTagTable().getValue("0008,0020");
        String studyTimeStr = (String) fileInfoDicom.getTagTable().getValue("0008,0030");
        String patientName = (String) fileInfoDicom.getTagTable().getValue("0010,0010");
        String coreSegVol = format.format(coreVolCC);
        
        //String reportTxt = "<html>\n";
        String reportTxt = "";
        reportTxt += "<h1>" + "MIPAV Stroke Core Segmentation Report" + "</h1>\n";
        reportTxt += "<ul>\n";
        reportTxt += "<li>" + "<b>" + "Time of segmentation run: " + "</b>" + curDateTimeStr + "</li>\n";
        reportTxt += "<li>" + "<b>" + "Study date and time: " + "</b>" + convertDateTimeToISOFormat(studyDateStr, studyTimeStr) + "</li>\n";
        reportTxt += "<li>" + "<b>" + "Patient last name initial: " + "</b>" + getInitialFromName(patientName) + "</li>\n";
        reportTxt += "<li>" + "<b>" + "Core segmentation volume (CC): " + "</b>" + coreSegVol + "</li>\n";
        //reportTxt += "<li>" + "<b>" + "" + "</b>" + "" + "</li>";
        reportTxt += "</ul>\n";
        reportTxt += "<h3>" + "DWI volume with core segmentation" + "</h3>\n";
        //reportTxt += "<a href='" + dwiPdfImage + "'><img src='" + dwiPdfImage + "' alt='DWI volume with core segmentation' width='" + imgDisplay + "'/></a>\n";
        reportTxt += "<img src='" + reportDwiCid + "' alt='DWI volume with core segmentation'/>\n";
        reportTxt += "<h3>" + "ADC volume with thresholded regions" + "</h3>\n";
        //reportTxt += "<a href='" + adcPdfImage + "'><img src='" + adcPdfImage + "' alt='ADC volume with thresholded regions' width='" + imgDisplay + "'/></a>\n";
        reportTxt += "<img src='" + reportAdcCid + "' alt='ADC volume with thresholded regions'/>\n";
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
    
    private boolean readEmailConfig() {
        final InputStream in = getClass().getResourceAsStream(configFileName);
        if (in != null) {
            final Properties prop = new Properties();
            try {
                prop.load(in);
            } catch (final IOException e) {
                Preferences.debug("Unable to load stroke segementation listener plugin preferences file: " + configFileName + "\n", Preferences.DEBUG_MINOR);
                e.printStackTrace();
                return false;
            }
            
            emailFrom = prop.getProperty("emailFrom");
            if (emailFrom == null || emailFrom.equals("")) {
                return false;
            }
            
            emailTo = prop.getProperty("emailTo");
            if (emailTo == null || emailTo.equals("")) {
                return false;
            }
            
            emailUsername = prop.getProperty("emailUsername");
            if (emailUsername == null || emailUsername.equals("")) {
                return false;
            }
            
            emailPassword = prop.getProperty("emailPassword");
            if (emailPassword == null || emailPassword.equals("")) {
                return false;
            }
            
            emailHost = prop.getProperty("emailHost");
            if (emailHost == null || emailHost.equals("")) {
                return false;
            }
            
            emailPort = prop.getProperty("emailPort");
            if (emailPort == null || emailPort.equals("")) {
                return false;
            }
            
            return true;
        } else {
            // couldn't load file
            return false;
        }
    }
}