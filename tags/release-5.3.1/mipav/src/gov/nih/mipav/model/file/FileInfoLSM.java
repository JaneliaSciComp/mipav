package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;


/**
 * This structures contains the information that describes how a LSM image is stored on disk.
 *
 * @see  FileLSM
 */

public class FileInfoLSM extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4011175280016387707L;

    /** Bleached ROI shapes. */
    public static final int RECTANGLE = 18;

    /** DOCUMENT ME! */
    public static final int ELLIPSE = 19;

    /** DOCUMENT ME! */
    public static final int CLOSED_POLYLINE = 20;

    /** DOCUMENT ME! */
    public static final int CLOSED_BEZIER = 22;

    /** DOCUMENT ME! */
    public static final int CIRCLE = 24;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int bleachedROIShape = -1;

    /** DOCUMENT ME! */
    private int[] blueArray = null;

    /** DOCUMENT ME! */
    private int[] channelDataTypes = null;

    /** DOCUMENT ME! */
    private String[] channelNames = null;

    /** DOCUMENT ME! */
    private int channels = -1;

    /** DOCUMENT ME! */
    private double displayAspectTime = 0.0;

    /** DOCUMENT ME! */
    private double displayAspectX = 0.0;

    /** DOCUMENT ME! */
    private double displayAspectY = 0.0;

    /** DOCUMENT ME! */
    private double displayAspectZ = 0.0;

    /** DOCUMENT ME! */
    private String[] eventDescription = null;

    /** DOCUMENT ME! */
    private double[] eventTime = null;

    /** DOCUMENT ME! */
    private int[] eventType = null;

    /** DOCUMENT ME! */
    private int firstSliceAfterBleach = -1;

    /** DOCUMENT ME! */
    private int[] greenArray = null;

    @SuppressWarnings("unused")
    private String imageDescription = null;

    /** DOCUMENT ME! */
    private double[] knotX = null;

    /** DOCUMENT ME! */
    private double[] knotY = null;

    /** DOCUMENT ME! */
    private int lastSliceBeforeBleach = -1;

    /** DOCUMENT ME! */
    private int LSMDataType = -1;

    /** DOCUMENT ME! */
    private int LSMDataType2 = -1;

    /** DOCUMENT ME! */
    private int mono = -1;

    /** DOCUMENT ME! */
    private double objectiveSphereCorrection = -1.0;

    /** DOCUMENT ME! */
    private int[] redArray = null;

    /** DOCUMENT ME! */
    private int scanType = -1;

    /** DOCUMENT ME! */
    private int spectralScan = -1;

    /** DOCUMENT ME! */
    private int timeDim = -1;

    /** DOCUMENT ME! */
    private double timeInterval = 0.0;

    /** DOCUMENT ME! */
    private double[] timeStamp = null;

    /** DOCUMENT ME! */
    private double[] wavelengths = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * FileInfoLSM - file info storage constructor.
     *
     * @param  name       file name
     * @param  directory  directory
     * @param  format     file format
     */
    public FileInfoLSM(String name, String directory, int format) {
        super(name, directory, format);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * displayAboutInfo - displays the file information.
     *
     * @param  dlog    dialog box that is written to
     * @param  matrix  transformation matrix
     */
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix) {
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
        } else if (LSMDataType == 1) {
            dialog.append("8-bit unsigned integers\n");
        } else if (LSMDataType == 2) {
            dialog.append("12-bit unsigned integers\n");
        } else if (LSMDataType == 5) {
            dialog.append("32-bit floats\n");
        }

        if (scanType == 0) {
            dialog.append("Normal x-y-z-scan\n");
        } else if (scanType == 1) {
            dialog.append("z-Scan (x-z-plane)\n");
        } else if (scanType == 2) {
            dialog.append("Line scan\n");
        } else if (scanType == 3) {
            dialog.append("Time series x-y\n");
        } else if (scanType == 4) {
            dialog.append("Time series x-z\n");
        } else if (scanType == 5) {
            dialog.append("Time series - Mean of ROIs\n");
        } else if (scanType == 6) {
            dialog.append("Time series x-y-z\n");
        } else if (scanType == 7) {
            dialog.append("Spline scan\n");
        } else if (scanType == 8) {
            dialog.append("Spline plane x-z\n");
        } else if (scanType == 9) {
            dialog.append("Time series spline plane x-z\n");
        } else if (scanType == 10) {
            dialog.append("Point mode\n");
        }

        if (spectralScan == 0) {
            dialog.append("No spectral scan\n");
        } else if (spectralScan == 1) {
            dialog.append("Image has been acquired in spectral scan mode with a ");
            dialog.append("Meta detector\n");
        }

        if (LSMDataType2 == 0) {
            dialog.append("Scan data\n");
        } else if (LSMDataType2 == 1) {
            dialog.append("Calculated data\n");
        } else if (LSMDataType2 == 2) {
            dialog.append("Animation\n");
        }

        if (timeInterval > 0.0) {
            dialog.append("Time interval for time series: " + timeInterval + "\n");
        }

        if (redArray != null) {

            for (int i = 0; i < redArray.length; i++) {
                dialog.append("Channel # " + i + " red = " + redArray[i] + " green = " + greenArray[i] + " blue = " +
                              blueArray[i] + "\n");
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
        } else if (mono > 0) {
            dialog.append("Mono button was pressed\n");
        }

        if (channelDataTypes != null) {

            for (int i = 0; i < channelDataTypes.length; i++) {

                if (channelDataTypes[i] == 1) {
                    dialog.append("Channel # " + i + " has 8-bit unsigned integers\n");
                } else if (channelDataTypes[i] == 2) {
                    dialog.append("Channel # " + i + " has 12-bit unsigned integers\n");
                } else if (channelDataTypes[i] == 5) {
                    dialog.append("Channel # " + i + " has 32-bit floats\n");
                }
            }
        } // if (channelDataTypes != null)

        if (timeStamp != null) {

            for (int i = 0; i < timeStamp.length; i++) {
                dialog.append("timeStamp[" + (i + 1) + "]: " + timeStamp[i] + "\n");
            }
        }

        if (eventTime != null) {

            for (int i = 0; i < eventTime.length; i++) {
                dialog.append("Event[" + (i + 1) + "]: " + eventTime[i] + "\n");

                if (eventType[i] == 0) {
                    dialog.append("Event[" + (i + 1) + "]: type = experimental annotation\n");
                } else if (eventType[i] == 1) {
                    dialog.append("Event[" + (i + 1) + "]: type = time interval has changed\n");
                } else if (eventType[i] == 2) {
                    dialog.append("Event[" + (i + 1) + "]: type = start of bleach operartion\n");
                } else if (eventType[i] == 3) {
                    dialog.append("Event[" + (i + 1) + "]: type = end of bleach operation\n");
                } else if (eventType[i] == 4) {
                    dialog.append("Event[" + (i + 1) + "]: type = trigger signal was detected\n");
                }

                if (eventDescription[i] != null) {
                    dialog.append("Event[" + (i + 1) + "]: description:\n" + eventDescription[i] + "\n");
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

            switch (bleachedROIShape) {

                case RECTANGLE:
                    dialog.append("The bleached ROI is a rectangle\n");
                    if ((knotX != null) && (knotY != null)) {
                        dialog.append("The first of 2 diagonally opposite corners " +
                                      "is located at approximately x = " + knotX[0] + " y = " + knotY[0] + "\n");
                        dialog.append("The second of 2 diagonally opposite corners " +
                                      "is located at approximately x = " + knotX[1] + " y = " + knotY[1] + "\n");
                    }

                    break;

                case ELLIPSE:
                    dialog.append("The bleached ROI is an ellipse\n");
                    if ((knotX != null) && (knotY != null) && (knotX.length >= 4) && (knotY.length >= 4)) {
                        dialog.append("One axis intercepts the ellipse " + "at approximately x = " + knotX[0] +
                                      " y = " + knotY[0] + " and x = " + knotX[2] + " y = " + knotY[2] + "\n");
                        dialog.append("The other axis intercepts the ellipse " + "at approximately x = " + knotX[1] +
                                      " y = " + knotY[1] + " and x = " + knotX[3] + " y = " + knotY[3] + "\n");
                    }

                    break;

                case CLOSED_POLYLINE:
                    dialog.append("The bleached ROI is a closed polyline\n");
                    if ((knotX != null) && (knotY != null)) {

                        for (int i = 0; i < knotX.length; i++) {
                            dialog.append("Vertex[" + (i + 1) + "] is located at approximately x = " + knotX[i] +
                                          " y = " + knotY[i] + "\n");
                        }
                    }

                    break;

                case CLOSED_BEZIER:
                    dialog.append("The bleached ROI is a closed bezier spline curve\n");
                    if ((knotX != null) && (knotY != null)) {

                        for (int i = 0; i < knotX.length; i++) {
                            dialog.append("Knot[" + (i + 1) + "] is located at approximately x = " + knotX[i] +
                                          " y = " + knotY[i] + "\n");
                        }
                    }

                    break;

                case CIRCLE:
                    dialog.append("The bleached ROI is a circle\n");
                    if ((knotX != null) && (knotY != null)) {

                        if (knotX.length == 2) {
                            dialog.append("The center is located at approximately x = " + knotX[0] + " y = " +
                                          knotY[0] + "\n");

                            double radius = Math.sqrt(((knotX[1] - knotX[0]) * (knotX[1] - knotX[0])) +
                                                      ((knotY[1] - knotY[0]) * (knotY[1] - knotY[0])));
                            dialog.append("The radius approximately = " + radius + "\n");
                        } // if (knotX.length == 2)
                        else if (knotX.length == 3) {

                            for (int i = 0; i < 3; i++) {
                                dialog.append("The circle has perimiter point " + (i + 1) + " at approximately x = " +
                                              knotX[i] + " y = " + knotY[i] + "\n");
                            }
                        }
                    }

                    break;
            }
        }

        if (displayAspectX > 0.0) {
            dialog.append("Zoom factor for image display in x-direction = " + displayAspectX + "\n");
        }

        if (displayAspectY > 0.0) {
            dialog.append("Zoom factor for image display in y-direction = " + displayAspectY + "\n");
        }

        if (displayAspectZ > 0.0) {
            dialog.append("Zoom factor for image display in z-direction = " + displayAspectZ + "\n");
        }

        if (displayAspectTime > 0.0) {
            dialog.append("Zoom factor for image display in time-direction = " + displayAspectTime + "\n");
        }

        if (objectiveSphereCorrection >= 0.0) {
            dialog.append("Objective sphere correction = " + objectiveSphereCorrection + "\n");
        }

        if (wavelengths != null) {

            for (int i = 0; i < (wavelengths.length / 2); i++) {
                dialog.append("Start wavelength[" + (i + 1) + "]: " + wavelengths[2 * i] + "\n");
                dialog.append("End wavelength[" + (i + 1) + "]: " + wavelengths[(2 * i) + 1] + "\n");
            }
        } // if (wavelengths != null)
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getBleachedROIShape() {
        return bleachedROIShape;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int[] getEventType() {
        return eventType;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getFirstSliceAfterBleach() {
        return firstSliceAfterBleach;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public double[] getKnotX() {
        return knotX;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public double[] getKnotY() {
        return knotY;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getLastSliceBeforeBleach() {
        return lastSliceBeforeBleach;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public double[] getTimeStamp() {
        return timeStamp;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  bleachedROIShape  DOCUMENT ME!
     */
    public void setBleachedROIShape(int bleachedROIShape) {
        this.bleachedROIShape = bleachedROIShape;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  blueArray  DOCUMENT ME!
     */
    public void setBlueArray(int[] blueArray) {
        this.blueArray = blueArray;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  channelDataTypes  DOCUMENT ME!
     */
    public void setChannelDataTypes(int[] channelDataTypes) {
        this.channelDataTypes = channelDataTypes;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  channelNames  DOCUMENT ME!
     */
    public void setChannelNames(String[] channelNames) {
        this.channelNames = channelNames;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  channels  DOCUMENT ME!
     */
    public void setChannels(int channels) {
        this.channels = channels;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  displayAspectTime  DOCUMENT ME!
     */
    public void setDisplayAspectTime(double displayAspectTime) {
        this.displayAspectTime = displayAspectTime;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  displayAspectX  DOCUMENT ME!
     */
    public void setDisplayAspectX(double displayAspectX) {
        this.displayAspectX = displayAspectX;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  displayAspectY  DOCUMENT ME!
     */
    public void setDisplayAspectY(double displayAspectY) {
        this.displayAspectY = displayAspectY;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  displayAspectZ  DOCUMENT ME!
     */
    public void setDisplayAspectZ(double displayAspectZ) {
        this.displayAspectZ = displayAspectZ;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  eventDescription  DOCUMENT ME!
     */
    public void setEventDescription(String[] eventDescription) {
        this.eventDescription = eventDescription;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  eventTime  DOCUMENT ME!
     */
    public void setEventTime(double[] eventTime) {
        this.eventTime = eventTime;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  eventType  DOCUMENT ME!
     */
    public void setEventType(int[] eventType) {
        this.eventType = eventType;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  firstSliceAfterBleach  DOCUMENT ME!
     */
    public void setFirstSliceAfterBleach(int firstSliceAfterBleach) {
        this.firstSliceAfterBleach = firstSliceAfterBleach;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  greenArray  DOCUMENT ME!
     */
    public void setGreenArray(int[] greenArray) {
        this.greenArray = greenArray;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  imageDescription  DOCUMENT ME!
     */
    public void setImageDescription(String imageDescription) {
        this.imageDescription = imageDescription;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  knotX  DOCUMENT ME!
     */
    public void setKnotX(double[] knotX) {
        this.knotX = knotX;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  knotY  DOCUMENT ME!
     */
    public void setKnotY(double[] knotY) {
        this.knotY = knotY;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  lastSliceBeforeBleach  DOCUMENT ME!
     */
    public void setLastSliceBeforeBleach(int lastSliceBeforeBleach) {
        this.lastSliceBeforeBleach = lastSliceBeforeBleach;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  LSMDataType  DOCUMENT ME!
     */
    public void setLSMDataType(int LSMDataType) {
        this.LSMDataType = LSMDataType;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  LSMDataType2  DOCUMENT ME!
     */
    public void setLSMDataType2(int LSMDataType2) {
        this.LSMDataType2 = LSMDataType2;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  mono  DOCUMENT ME!
     */
    public void setMono(int mono) {
        this.mono = mono;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  objectiveSphereCorrection  DOCUMENT ME!
     */
    public void setObjectiveSphereCorrection(double objectiveSphereCorrection) {
        this.objectiveSphereCorrection = objectiveSphereCorrection;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  redArray  DOCUMENT ME!
     */
    public void setRedArray(int[] redArray) {
        this.redArray = redArray;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  scanType  DOCUMENT ME!
     */
    public void setScanType(int scanType) {
        this.scanType = scanType;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  spectralScan  DOCUMENT ME!
     */
    public void setSpectralScan(int spectralScan) {
        this.spectralScan = spectralScan;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  timeDim  DOCUMENT ME!
     */
    public void setTimeDim(int timeDim) {
        this.timeDim = timeDim;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  timeInterval  DOCUMENT ME!
     */
    public void setTimeInterval(double timeInterval) {
        this.timeInterval = timeInterval;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  timeStamp  DOCUMENT ME!
     */
    public void setTimeStamp(double[] timeStamp) {
        this.timeStamp = timeStamp;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  wavelengths  DOCUMENT ME!
     */
    public void setWavelengths(double[] wavelengths) {
        this.wavelengths = wavelengths;
    }
}
