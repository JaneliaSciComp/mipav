package gov.nih.mipav.model.file;

import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.model.structures.*;

/**
*   This structures contains the information that describes how
*   a LSM image is stored on disk.
*
*       @see        FileLSM
*
*/

public class FileInfoLSM extends FileInfoBase {

    // Bleached ROI shapes
    public static final int RECTANGLE = 18;
    public static final int ELLIPSE = 19;
    public static final int CLOSED_POLYLINE = 20;
    public static final int CLOSED_BEZIER = 22;
    public static final int CIRCLE = 24;

    private String imageDescription = null;
    private int channels = -1;
    private int timeDim = -1;
    private int LSMDataType = -1;
    private int scanType = -1;
    private int spectralScan = -1;
    private int LSMDataType2 = -1;
    private double timeInterval = 0.0;
    private int [] channelDataTypes = null;
    private double [] timeStamp = null;
    private double [] eventTime = null;
    private int [] eventType = null;
    private String [] eventDescription = null;
    private int [] redArray = null;
    private int [] greenArray = null;
    private int [] blueArray = null;
    private String [] channelNames = null;
    private int mono = -1;
    private double [] wavelengths = null;
    private int lastSliceBeforeBleach = -1;
    private int firstSliceAfterBleach = -1;
    private int bleachedROIShape = -1;
    private double knotX[] = null;
    private double knotY[] = null;
    private double displayAspectX = 0.0;
    private double displayAspectY = 0.0;
    private double displayAspectZ = 0.0;
    private double displayAspectTime = 0.0;
    private double objectiveSphereCorrection = -1.0;


    /**
    *  FileInfoLSM     - file info storage constructor
    *  @param name        file name
    *  @param directory   directory
    *  @param format      file format
    */
    public FileInfoLSM(String name, String directory, int format) {
        super(name, directory, format);
    }

    public void setImageDescription(String imageDescription) {
        this.imageDescription = imageDescription;
    }

    public void setChannels(int channels) {
        this.channels = channels;
    }

    public void setTimeDim(int timeDim) {
        this.timeDim = timeDim;
    }

    public void setLSMDataType(int LSMDataType) {
        this.LSMDataType = LSMDataType;
    }

    public void setScanType(int scanType) {
        this.scanType = scanType;
    }

    public void setSpectralScan(int spectralScan) {
        this.spectralScan = spectralScan;
    }

    public void setLSMDataType2(int LSMDataType2) {
        this.LSMDataType2 = LSMDataType2;
    }

    public void setTimeInterval(double timeInterval) {
        this.timeInterval = timeInterval;
    }

    public void setChannelDataTypes(int[] channelDataTypes) {
        this.channelDataTypes = channelDataTypes;
    }

    public void setTimeStamp (double[] timeStamp) {
        this.timeStamp = timeStamp;
    }

    public double[] getTimeStamp() {
        return timeStamp;
    }

    public void setEventTime(double[] eventTime) {
        this.eventTime = eventTime;
    }

    public void setEventType(int[] eventType) {
        this.eventType = eventType;
    }

    public int[] getEventType() {
        return eventType;
    }

    public void setEventDescription(String[] eventDescription) {
        this.eventDescription = eventDescription;
    }

    public void setRedArray(int[] redArray) {
        this.redArray = redArray;
    }

    public void setGreenArray(int[] greenArray) {
        this.greenArray = greenArray;
    }

    public void setBlueArray(int[] blueArray) {
        this.blueArray = blueArray;
    }

    public void setChannelNames(String[] channelNames) {
        this.channelNames = channelNames;
    }

    public void setMono(int mono) {
        this.mono = mono;
    }

    public void setDisplayAspectX(double displayAspectX) {
        this.displayAspectX = displayAspectX;
    }

    public void setDisplayAspectY(double displayAspectY) {
        this.displayAspectY = displayAspectY;
    }

    public void setDisplayAspectZ(double displayAspectZ) {
        this.displayAspectZ = displayAspectZ;
    }

    public void setDisplayAspectTime(double displayAspectTime) {
        this.displayAspectTime = displayAspectTime;
    }

    public void setWavelengths(double wavelengths[]) {
        this.wavelengths = wavelengths;
    }

    public void setObjectiveSphereCorrection(double objectiveSphereCorrection) {
        this.objectiveSphereCorrection = objectiveSphereCorrection;
    }

    public void setLastSliceBeforeBleach(int lastSliceBeforeBleach) {
        this.lastSliceBeforeBleach = lastSliceBeforeBleach;
    }

    public int getLastSliceBeforeBleach() {
        return lastSliceBeforeBleach;
    }

    public void setFirstSliceAfterBleach(int firstSliceAfterBleach) {
        this.firstSliceAfterBleach = firstSliceAfterBleach;
    }

    public int getFirstSliceAfterBleach() {
        return firstSliceAfterBleach;
    }

    public void setBleachedROIShape(int bleachedROIShape) {
        this.bleachedROIShape = bleachedROIShape;
    }

    public int getBleachedROIShape() {
        return bleachedROIShape;
    }

    public void setKnotX(double[] knotX) {
        this.knotX = knotX;
    }

    public double[] getKnotX() {
        return knotX;
    }

    public void setKnotY(double[] knotY) {
        this.knotY = knotY;
    }

    public double[] getKnotY() {
        return knotY;
    }

    /**
    *  displayAboutInfo - displays the file information
    *  @param dlog    dialog box that is written to
    *  @param matrix  transformation matrix
    */
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix){
        JDialogText dialog = (JDialogText) dlog;
        displayPrimaryInfo(dialog, matrix);
        dialog.append("\n\n                Other information\n\n");
        if (channels >= 0) {
            dialog.append("Number of channels: " + channels + "\n");
        }

        if (timeDim >= 0) {
            dialog.append("Time dimension = " + timeDim + "\n");
        }

        if (LSMDataType == 0) {
            dialog.append("Different channels have different numbers of data bits\n");
        }
        else if (LSMDataType == 1) {
            dialog.append("8-bit unsigned integers\n");
        }
        else if (LSMDataType == 2) {
            dialog.append("12-bit unsigned integers\n");
        }
        else if (LSMDataType == 5) {
            dialog.append("32-bit floats\n");
        }

        if (scanType == 0) {
            dialog.append("Normal x-y-z-scan\n");
        }
        else if (scanType == 1) {
            dialog.append("z-Scan (x-z-plane)\n");
        }
        else if (scanType == 2) {
            dialog.append("Line scan\n");
        }
        else if (scanType == 3) {
            dialog.append("Time series x-y\n");
        }
        else if (scanType == 4) {
            dialog.append("Time series x-z\n");
        }
        else if (scanType == 5) {
            dialog.append("Time series - Mean of ROIs\n");
        }
        else if (scanType == 6) {
            dialog.append("Time series x-y-z\n");
        }
        else if (scanType == 7) {
            dialog.append("Spline scan\n");
        }
        else if (scanType == 8) {
            dialog.append("Spline plane x-z\n");
        }
        else if (scanType == 9) {
            dialog.append("Time series spline plane x-z\n");
        }
        else if (scanType == 10) {
            dialog.append("Point mode\n");
        }

        if (spectralScan == 0) {
            dialog.append("No spectral scan\n");
        }
        else if (spectralScan == 1) {
            dialog.append("Image has been acquired in spectral scan mode with a ");
            dialog.append("Meta detector\n");
        }

        if (LSMDataType2 == 0) {
            dialog.append("Scan data\n");
        }
        else if (LSMDataType2 == 1) {
            dialog.append("Calculated data\n");
        }
        else if (LSMDataType2 == 2) {
            dialog.append("Animation\n");
        }

        if (timeInterval > 0.0) {
            dialog.append("Time interval for time series: " + timeInterval + "\n");
        }

        if (redArray != null) {
            for (int i = 0; i < redArray.length; i++) {
                dialog.append("Channel # " + i + " red = " + redArray[i] + " green = " +
                               greenArray[i] + " blue = " + blueArray[i] + "\n");
            }
        }

        if (channelNames != null) {
            for (int i = 0; i < channelNames.length; i++) {
                if (channelNames[i] != null) {
                    dialog.append("Channel # " + i + " Name = " + channelNames[i] + "\n");
                }
            }
        }

        if (mono == 0) {
            dialog.append("Mono button was not pressed\n");
        }
        else if (mono > 0) {
            dialog.append("Mono button was pressed\n");
        }

        if (channelDataTypes != null) {
            for (int i = 0; i < channelDataTypes.length; i++) {
                if (channelDataTypes[i] == 1) {
                    dialog.append("Channel # " + i + " has 8-bit unsigned integers\n");
                }
                else if (channelDataTypes[i] == 2) {
                    dialog.append("Channel # " + i + " has 12-bit unsigned integers\n");
                }
                else if (channelDataTypes[i] == 5) {
                    dialog.append("Channel # " + i + " has 32-bit floats\n");
                }
            }
        } // if (channelDataTypes != null)

        if (timeStamp != null) {
            for (int i = 0; i < timeStamp.length; i++) {
                dialog.append("timeStamp[" +(i+1) + "]: " + timeStamp[i] + "\n");
            }
        }

        if (eventTime != null) {
            for (int i = 0; i < eventTime.length; i++) {
                dialog.append("Event[" + (i+1) + "]: " + eventTime[i] + "\n");
                if (eventType[i] == 0) {
                    dialog.append("Event[" + (i+1) + "]: type = experimental annotation\n");
                }
                else if (eventType[i] == 1) {
                    dialog.append("Event[" + (i+1) + "]: type = time interval has changed\n");
                }
                else if (eventType[i] == 2) {
                    dialog.append("Event[" + (i+1) + "]: type = start of bleach operartion\n");
                }
                else if (eventType[i] == 3) {
                    dialog.append("Event[" + (i+1) + "]: type = end of bleach operation\n");
                }
                else if (eventType[i] == 4) {
                    dialog.append("Event[" + (i+1) + "]: type = trigger signal was detected\n");
                }
                if (eventDescription[i] != null) {
                    dialog.append("Event[" + (i+1) + "]: description:\n" +
                                   eventDescription[i] + "\n");
                }
            }
        } // if (eventTime != null)

        if (lastSliceBeforeBleach >= 0) {
            dialog.append("Last slice before bleach = " + (lastSliceBeforeBleach + 1) + "\n");
        }

        if (firstSliceAfterBleach >= 0) {
            dialog.append("First slice after bleach = " + (firstSliceAfterBleach + 1) + "\n");
        }

        if (bleachedROIShape >= 0) {
            switch(bleachedROIShape) {
                case RECTANGLE:
                    dialog.append("The bleached ROI is a rectangle\n");
                    if ((knotX != null) && (knotY != null)) {
                        dialog.append("The first of 2 diagonally opposite corners " +
                        "is located at approximately x = " +
                        knotX[0] + " y = " + knotY[0] + "\n");
                        dialog.append("The second of 2 diagonally opposite corners " +
                        "is located at approximately x = " +
                        knotX[1] + " y = " + knotY[1] + "\n");
                    }
                    break;
                case ELLIPSE:
                    dialog.append("The bleached ROI is an ellipse\n");
                    if ((knotX != null) && (knotY != null) &&
                        (knotX.length >= 4) && (knotY.length >= 4)) {
                        dialog.append("One axis intercepts the ellipse " +
                        "at approximately x = " +
                        knotX[0] + " y = " + knotY[0] + " and x = " + knotX[2] + " y = " +
                        knotY[2] + "\n");
                        dialog.append("The other axis intercepts the ellipse " +
                        "at approximately x = " +
                        knotX[1] + " y = " + knotY[1] + " and x = " + knotX[3] + " y = " +
                        knotY[3] + "\n");
                    }
                    break;
                case CLOSED_POLYLINE:
                    dialog.append("The bleached ROI is a closed polyline\n");
                    if ((knotX != null) && (knotY != null)) {
                        for (int i = 0; i < knotX.length; i++) {
                            dialog.append("Vertex[" + (i+1) +
                                          "] is located at approximately x = " +
                                          knotX[i] + " y = " + knotY[i] + "\n");
                        }
                    }
                    break;
                case CLOSED_BEZIER:
                    dialog.append("The bleached ROI is a closed bezier spline curve\n");
                    if ((knotX != null) && (knotY != null)) {
                        for (int i = 0; i < knotX.length; i++) {
                            dialog.append("Knot[" + (i+1) +
                                          "] is located at approximately x = " +
                                          knotX[i] + " y = " + knotY[i] + "\n");
                        }
                    }
                    break;
                case CIRCLE:
                    dialog.append("The bleached ROI is a circle\n");
                    if ((knotX != null) && (knotY != null)) {
                        if (knotX.length == 2) {
                            dialog.append("The center is located at approximately x = " +
                                        knotX[0] + " y = " + knotY[0] + "\n");
                            double radius = Math.sqrt(
                            (knotX[1] - knotX[0])*(knotX[1] - knotX[0])
                            +  (knotY[1] - knotY[0])*(knotY[1] - knotY[0]));
                            dialog.append("The radius approximately = " + radius + "\n");
                        } // if (knotX.length == 2)
                        else if (knotX.length == 3) {
                            for (int i = 0; i < 3; i++) {
                            dialog.append(
                            "The circle has perimiter point " + (i+1) +
                            " at approximately x = " +
                            knotX[i] + " y = " + knotY[i] + "\n");
                            }
                        }
                    }
                    break;
            }
        }

        if (displayAspectX > 0.0) {
            dialog.append("Zoom factor for image display in x-direction = " +
                           displayAspectX + "\n");
        }

        if (displayAspectY > 0.0) {
            dialog.append("Zoom factor for image display in y-direction = " +
                           displayAspectY + "\n");
        }

        if (displayAspectZ > 0.0) {
            dialog.append("Zoom factor for image display in z-direction = " +
                           displayAspectZ + "\n");
        }

        if (displayAspectTime > 0.0) {
            dialog.append("Zoom factor for image display in time-direction = " +
                           displayAspectTime + "\n");
        }

        if (objectiveSphereCorrection >= 0.0) {
            dialog.append("Objective sphere correction = " +
                           objectiveSphereCorrection + "\n");
        }

        if (wavelengths != null) {
            for (int i = 0; i < wavelengths.length/2; i++) {
                dialog.append("Start wavelength[" + (i+1) + "]: " + wavelengths[2*i] + "\n");
                dialog.append("End wavelength[" + (i+1) + "]: " + wavelengths[2*i+1] + "\n");
            }
        } // if (wavelengths != null)
    }
}
