package gov.nih.mipav.model.file;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.io.Serializable;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.CharsetDecoder;


import java.lang.reflect.Array;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import gov.nih.mipav.model.file.MetadataExtractorTest.CanonMakernoteDescriptorTest;

import java.util.Iterator;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.Collection;
import java.util.TimeZone;


import java.math.RoundingMode;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

/*
 * Ported to MIPAV by William Gandler
 * Copyright 2002-2019 Drew Noakes and contributors
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 *
 * More information about this project is available at:
 *
 *    https://drewnoakes.com/code/exif/
 *    https://github.com/drewnoakes/metadata-extractor
 */


public class MetadataExtractorTest extends MetadataExtractor {
	
	public MetadataExtractorTest() {
		
	}
	
	/**
	 * @author Drew Noakes https://drewnoakes.com
	 */
	public class CanonMakernoteDescriptorTest
	{
		public CanonMakernoteDescriptorTest() {
			
		}
	    //@Test
		//MetadataExtractorTest me = new MetadataExtractorTest();
    	//CanonMakernoteDescriptorTest cm = me.new CanonMakernoteDescriptorTest();
    	//try {
    	//    cm.testGetFlashBiasDescription();
    	//}
    	//catch (Exception e) {
    	//	e.printStackTrace();
    	//}
		//Finished running testGetFlashBiasDescription()
	    public void testGetFlashBiasDescription() throws Exception
	    {
	        CanonMakernoteDirectory directory = new CanonMakernoteDirectory();
	        CanonMakernoteDescriptor descriptor = new CanonMakernoteDescriptor(directory);

	        // set and check values

	        directory.setInt(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS, 0xFFC0);
	        assertEquals("-2.0 EV", descriptor.getDescription(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS));

	        directory.setInt(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS, 0xffd4);
	        assertEquals("-1.375 EV", descriptor.getDescription(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS));

	        directory.setInt(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS, 0x0000);
	        assertEquals("0.0 EV", descriptor.getDescription(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS));

	        directory.setInt(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS, 0x000c);
	        assertEquals("0.375 EV", descriptor.getDescription(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS));

	        directory.setInt(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS, 0x0010);
	        assertEquals("0.5 EV", descriptor.getDescription(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS));

	        directory.setInt(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS, 0x0014);
	        assertEquals("0.625 EV", descriptor.getDescription(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS));

	        directory.setInt(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS, 0x0020);
	        assertEquals("1.0 EV", descriptor.getDescription(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS));

	        directory.setInt(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS, 0x0030);
	        assertEquals("1.5 EV", descriptor.getDescription(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS));

	        directory.setInt(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS, 0x0034);
	        assertEquals("1.625 EV", descriptor.getDescription(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS));

	        directory.setInt(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS, 0x0040);
	        assertEquals("2.0 EV", descriptor.getDescription(CanonMakernoteDirectory.FocalLength.TAG_FLASH_BIAS));
	        System.out.println("Finished running testGetFlashBiasDescription()");
	    }
	}
	
	/**
	 * JUnit test case for class ExifReader.
	 *
	 * @author Drew Noakes https://drewnoakes.com
	 */
	public class ExifReaderTest
	{
		//MetadataExtractorTest me = new MetadataExtractorTest();
    	//ExifReaderTest er = me.new ExifReaderTest();
		public ExifReaderTest() {
			
		}
	    @NotNull
	    public Metadata processBytes(@NotNull String filePath) throws IOException
	    {
	        Metadata metadata = new Metadata();
	        byte[] bytes = FileUtil.readBytes(filePath);
	        new ExifReader().extract(new ByteArrayReader(bytes), metadata, ExifReader.JPEG_SEGMENT_PREAMBLE.length(), null);
	        return metadata;
	    }

	    @NotNull
	    public <T extends Directory> T processBytes(@NotNull String filePath, @NotNull Class<T> directoryClass) throws IOException
	    {
	        T directory = processBytes(filePath).getFirstDirectoryOfType(directoryClass);
	        assertNotNull(directory);
	        return directory;
	    }

	    //@SuppressWarnings("ConstantConditions")
	    //@Test
	    //try {
	    //	er.testExtractWithNullDataThrows();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testExtractWithNullDataThrows()
	    public void testExtractWithNullDataThrows() throws Exception
	    {
	        try{
	            new ExifReader().readJpegSegments(null, new Metadata(), JpegSegmentType.APP1);
	            fail("Exception expected");
	        } catch (NullPointerException npe) {
	            // passed
	        }
	        System.out.println("Finished running testExtractWithNullDataThrows()");
	    }

	    //@Test
	    //try {
	    //	er.testLoadFujifilmJpeg();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testLoadFujifilmJpeg()
	    public void testLoadFujifilmJpeg() throws Exception
	    {
	        ExifSubIFDDirectory directory = processBytes("C:/metadata/metadata-extractor-master/Tests/Data/withExif.jpg.app1", ExifSubIFDDirectory.class);

	        final String description = directory.getDescription(ExifSubIFDDirectory.TAG_ISO_EQUIVALENT);
	        assertNotNull(description);
	        assertEquals("80", description);
	        System.out.println("Finished running testLoadFujifilmJpeg()");
	        // TODO decide if this should still be returned -- it was being calculated upon setting of a related tag
//	      assertEquals("F9", directory.getDescription(ExifSubIFDDirectory.TAG_APERTURE));
	    }

	    //@Test
	    //try {
	    //	er.testReadJpegSegmentWithNoExifData();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testReadJpegSegmentWithNoExifData()
	    public void testReadJpegSegmentWithNoExifData() throws Exception
	    {
	        byte[] badExifData = new byte[]{ 1,2,3,4,5,6,7,8,9,10 };
	        Metadata metadata = new Metadata();
	        ArrayList<byte[]> segments = new ArrayList<byte[]>();
	        segments.add(badExifData);
	        new ExifReader().readJpegSegments(segments, metadata, JpegSegmentType.APP1);
	        assertEquals(0, metadata.getDirectoryCount());
	        assertFalse(metadata.hasErrors());
	        System.out.println("Finished running testReadJpegSegmentWithNoExifData()");
	    }

	    //@Test
	    //try {
	    //	er.testCrashRegressionTest();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running CrashRegressionTest()
	    public void testCrashRegressionTest() throws Exception
	    {
	        // This image was created via a resize in ACDSee.
	        // It seems to have a reference to an IFD starting outside the data segment.
	        // I've noticed that ACDSee reports a Comment for this image, yet ExifReader doesn't report one.
	        ExifSubIFDDirectory directory = processBytes("C:/metadata/metadata-extractor-master/Tests/Data/crash01.jpg.app1", ExifSubIFDDirectory.class);

	        assertTrue(directory.getTagCount() > 0);
	        System.out.println("Finished running CrashRegressionTest()");
	    }

	    //@Test
	    //try {
	    //	er.testDateTime();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testDateTime()
	    public void testDateTime() throws Exception
	    {
	        ExifIFD0Directory directory = processBytes("C:/metadata/metadata-extractor-master/Tests/Data/manuallyAddedThumbnail.jpg.app1", ExifIFD0Directory.class);

	        assertEquals("2002:11:27 18:00:35", directory.getString(ExifIFD0Directory.TAG_DATETIME));
	        System.out.println("Finished running testDateTime()");
	    }

	    //@Test
	    //try {
	    //	er.testThumbnailXResolution();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testThumbnailXResolution()
	    public void testThumbnailXResolution() throws Exception
	    {
	        ExifThumbnailDirectory directory = processBytes("C:/metadata/metadata-extractor-master/Tests/Data/manuallyAddedThumbnail.jpg.app1", ExifThumbnailDirectory.class);

	        Rational rational = directory.getRational(ExifThumbnailDirectory.TAG_X_RESOLUTION);
	        assertNotNull(rational);
	        assertEquals(72, rational.getNumerator());
	        assertEquals(1, rational.getDenominator());
	        System.out.println("Finished running testThumbnailXResolution()");
	    }

	    //@Test
	    //try {
	    //	er.testThumbnailYResolution();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testThumbnailYResolution()
	    public void testThumbnailYResolution() throws Exception
	    {
	        ExifThumbnailDirectory directory = processBytes("C:/metadata/metadata-extractor-master/Tests/Data/manuallyAddedThumbnail.jpg.app1", ExifThumbnailDirectory.class);

	        Rational rational = directory.getRational(ExifThumbnailDirectory.TAG_Y_RESOLUTION);
	        assertNotNull(rational);
	        assertEquals(72, rational.getNumerator());
	        assertEquals(1, rational.getDenominator());
	        System.out.println("Finished running testThumbnailYResolution()");
	    }

	    //@Test
	    //try {
	    //	er.testThumbnailOffset();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testThumbnailOffset()
	    public void testThumbnailOffset() throws Exception
	    {
	        ExifThumbnailDirectory directory = processBytes("C:/metadata/metadata-extractor-master/Tests/Data/manuallyAddedThumbnail.jpg.app1", ExifThumbnailDirectory.class);

	        assertEquals(192, directory.getInt(ExifThumbnailDirectory.TAG_THUMBNAIL_OFFSET));
	        System.out.println("Finished running testThumbnailOffset()");
	    }

	    //@Test
	    //try {
	    //	er.testThumbnailLength();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testThumbnailLength()
	    public void testThumbnailLength() throws Exception
	    {
	        ExifThumbnailDirectory directory = processBytes("C:/metadata/metadata-extractor-master/Tests/Data/manuallyAddedThumbnail.jpg.app1", ExifThumbnailDirectory.class);

	        assertEquals(2970, directory.getInt(ExifThumbnailDirectory.TAG_THUMBNAIL_LENGTH));
	        System.out.println("Finished running testThumbnailLength()");
	    }

	    //@Test
	    //try {
	    //	er.testCompression();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testCompression()
	    public void testCompression() throws Exception
	    {
	        ExifThumbnailDirectory directory = processBytes("C:/metadata/metadata-extractor-master/Tests/Data/manuallyAddedThumbnail.jpg.app1", ExifThumbnailDirectory.class);

	        // 6 means JPEG compression
	        assertEquals(6, directory.getInt(ExifThumbnailDirectory.TAG_COMPRESSION));
	        System.out.println("Finished running testCompression()");
	    }

	    //@Test
	    //try {
	    //	er.testStackOverflowOnRevisitationOfSameDirectory();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testStackOverflowOnRevisitationOfSameDirectory()
	    public void testStackOverflowOnRevisitationOfSameDirectory() throws Exception
	    {
	        // An error has been discovered in Exif data segments where a directory is referenced
	        // repeatedly.  Thanks to Alistair Dickie for providing the sample data used in this
	        // unit test.

	        Metadata metadata = processBytes("C:/metadata/metadata-extractor-master/Tests/Data/recursiveDirectories.jpg.app1");

	        // Mostly we're just happy at this point that we didn't get stuck in an infinite loop.

	        assertEquals(5, metadata.getDirectoryCount());
	        System.out.println("Finished running testStackOverflowOnRevisitationOfSameDirectory()");
	    }

	    //@Test
	    //try {
	    //	er.testDifferenceImageAndThumbnailOrientations();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testDifferenceImageAndThumbnailOrientations()
	    public void testDifferenceImageAndThumbnailOrientations() throws Exception
	    {
	        // This metadata contains different orientations for the thumbnail and the main image.
	        // These values used to be merged into a single directory, causing errors.
	        // This unit test demonstrates correct behaviour.
	        Metadata metadata = processBytes("C:/metadata/metadata-extractor-master/Tests/Data/repeatedOrientationTagWithDifferentValues.jpg.app1");
	        ExifIFD0Directory ifd0Directory = metadata.getFirstDirectoryOfType(ExifIFD0Directory.class);
	        ExifThumbnailDirectory thumbnailDirectory = metadata.getFirstDirectoryOfType(ExifThumbnailDirectory.class);

	        assertNotNull(ifd0Directory);
	        assertNotNull(thumbnailDirectory);

	        assertEquals(1, ifd0Directory.getInt(ExifIFD0Directory.TAG_ORIENTATION));
	        assertEquals(8, thumbnailDirectory.getInt(ExifThumbnailDirectory.TAG_ORIENTATION));
	        System.out.println("Finished running testDifferenceImageAndThumbnailOrientations()");
	    }

	/*
	    public void testUncompressedYCbCrThumbnail() throws Exception
	    {
	        String fileName = "withUncompressedYCbCrThumbnail.jpg";
	        String thumbnailFileName = "withUncompressedYCbCrThumbnail.bmp";
	        Metadata metadata = new ExifReader(new File(fileName)).extract();
	        ExifSubIFDDirectory directory = (ExifSubIFDDirectory)metadata.getOrCreateDirectory(ExifSubIFDDirectory.class);
	        directory.writeThumbnail(thumbnailFileName);

	        fileName = "withUncompressedYCbCrThumbnail2.jpg";
	        thumbnailFileName = "withUncompressedYCbCrThumbnail2.bmp";
	        metadata = new ExifReader(new File(fileName)).extract();
	        directory = (ExifSubIFDDirectory)metadata.getOrCreateDirectory(ExifSubIFDDirectory.class);
	        directory.writeThumbnail(thumbnailFileName);
	        fileName = "withUncompressedYCbCrThumbnail3.jpg";
	        thumbnailFileName = "withUncompressedYCbCrThumbnail3.bmp";
	        metadata = new ExifReader(new File(fileName)).extract();
	        directory = (ExifSubIFDDirectory)metadata.getOrCreateDirectory(ExifSubIFDDirectory.class);
	        directory.writeThumbnail(thumbnailFileName);
	        fileName = "withUncompressedYCbCrThumbnail4.jpg";
	        thumbnailFileName = "withUncompressedYCbCrThumbnail4.bmp";
	        metadata = new ExifReader(new File(fileName)).extract();
	        directory = (ExifSubIFDDirectory)metadata.getOrCreateDirectory(ExifSubIFDDirectory.class);
	        directory.writeThumbnail(thumbnailFileName);
	    }

	    public void testUncompressedRGBThumbnail() throws Exception
	    {
	        String fileName = "withUncompressedRGBThumbnail.jpg";
	        String thumbnailFileName = "withUncompressedRGBThumbnail.bmp";
	        Metadata metadata = new ExifReader(new File(fileName)).extract();
	        ExifSubIFDDirectory directory = (ExifSubIFDDirectory)metadata.getOrCreateDirectory(ExifSubIFDDirectory.class);
	        directory.writeThumbnail(thumbnailFileName);
	    }
	*/
	}
	
	/**
	 * Unit tests for {@link ExifSubIFDDirectory}, {@link ExifIFD0Directory}, {@link ExifThumbnailDirectory} and
	 * {@link GpsDirectory}.
	 *
	 * @author Drew Noakes https://drewnoakes.com
	 */
	//@SuppressWarnings("ConstantConditions")
	public class ExifDirectoryTest
	{
		//MetadataExtractorTest me = new MetadataExtractorTest();
    	//ExifDirectoryTest ed = me.new ExifDirectoryTest();
		public ExifDirectoryTest() {
			
		}
		//@Test
		//try {
	    //	ed.testGetDirectoryName();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
		//Finished running testGetDirectoryName()
	    public void testGetDirectoryName() throws Exception
	    {
	        Directory subIFDDirectory = new ExifSubIFDDirectory();
	        Directory ifd0Directory = new ExifIFD0Directory();
	        Directory thumbDirectory = new ExifThumbnailDirectory();
	        Directory gpsDirectory = new GpsDirectory();

	        assertFalse(subIFDDirectory.hasErrors());
	        assertFalse(ifd0Directory.hasErrors());
	        assertFalse(thumbDirectory.hasErrors());
	        assertFalse(gpsDirectory.hasErrors());

	        assertEquals("Exif IFD0", ifd0Directory.getName());
	        assertEquals("Exif SubIFD", subIFDDirectory.getName());
	        assertEquals("Exif Thumbnail", thumbDirectory.getName());
	        assertEquals("GPS", gpsDirectory.getName());
	        System.out.println("Finished running testGetDirectoryName()");
	    }

	    //@Test
	    //try {
	    //	ed.testDateTime();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testDateTime()
	    public void testDateTime() throws JpegProcessingException, IOException, MetadataException
	    {
	    	ExifReaderTest er = new ExifReaderTest();
	        Metadata metadata = er.processBytes("C:/metadata/metadata-extractor-master/Tests/Data/nikonMakernoteType2a.jpg.app1");

	        ExifIFD0Directory exifIFD0Directory = metadata.getFirstDirectoryOfType(ExifIFD0Directory.class);
	        ExifSubIFDDirectory exifSubIFDDirectory = metadata.getFirstDirectoryOfType(ExifSubIFDDirectory.class);

	        assertNotNull(exifIFD0Directory);
	        assertNotNull(exifSubIFDDirectory);

	        assertEquals("2003:10:15 10:37:08", exifIFD0Directory.getString(ExifIFD0Directory.TAG_DATETIME));
	        assertEquals("80", exifSubIFDDirectory.getString(ExifSubIFDDirectory.TAG_SUBSECOND_TIME));
	        assertEquals("2003:10:15 10:37:08", exifSubIFDDirectory.getString(ExifSubIFDDirectory.TAG_DATETIME_ORIGINAL));
	        assertEquals("80", exifSubIFDDirectory.getString(ExifSubIFDDirectory.TAG_SUBSECOND_TIME_ORIGINAL));
	        assertEquals("2003:10:15 10:37:08", exifSubIFDDirectory.getString(ExifSubIFDDirectory.TAG_DATETIME_DIGITIZED));
	        assertEquals("80", exifSubIFDDirectory.getString(ExifSubIFDDirectory.TAG_SUBSECOND_TIME_DIGITIZED));

	        assertEquals(1066214228800L, exifIFD0Directory.getDate(
	            ExifIFD0Directory.TAG_DATETIME,
	            exifSubIFDDirectory.getString(ExifSubIFDDirectory.TAG_SUBSECOND_TIME),
	            null
	        ).getTime());
	        assertEquals(1066210628800L, exifIFD0Directory.getDate(
	            ExifIFD0Directory.TAG_DATETIME,
	            exifSubIFDDirectory.getString(ExifSubIFDDirectory.TAG_SUBSECOND_TIME),
	            TimeZone.getTimeZone("GMT+0100")
	        ).getTime());

	        assertEquals(1066214228800L, exifSubIFDDirectory.getDateModified().getTime());
	        assertEquals(1066210628800L, exifSubIFDDirectory.getDateModified(TimeZone.getTimeZone("GMT+0100")).getTime());
	        assertEquals(1066214228800L, exifSubIFDDirectory.getDateOriginal().getTime());
	        assertEquals(1066210628800L, exifSubIFDDirectory.getDateOriginal(TimeZone.getTimeZone("GMT+0100")).getTime());
	        assertEquals(1066214228800L, exifSubIFDDirectory.getDateDigitized().getTime());
	        assertEquals(1066210628800L, exifSubIFDDirectory.getDateDigitized(TimeZone.getTimeZone("GMT+0100")).getTime());
	        System.out.println("Finished running testDateTime()");
	    }

	    //@Test
	    //try {
	    //	ed.testResolution();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testResolution()
	    public void testResolution() throws JpegProcessingException, IOException, MetadataException
	    {
	    	ExifReaderTest er = new ExifReaderTest();
	        Metadata metadata = er.processBytes("C:/metadata/metadata-extractor-master/Tests/Data/withUncompressedRGBThumbnail.jpg.app1");

	        ExifThumbnailDirectory thumbnailDirectory = metadata.getFirstDirectoryOfType(ExifThumbnailDirectory.class);
	        assertNotNull(thumbnailDirectory);
	        assertEquals(72, thumbnailDirectory.getInt(ExifThumbnailDirectory.TAG_X_RESOLUTION));

	        ExifIFD0Directory exifIFD0Directory = metadata.getFirstDirectoryOfType(ExifIFD0Directory.class);
	        assertNotNull(exifIFD0Directory);
	        assertEquals(216, exifIFD0Directory.getInt(ExifIFD0Directory.TAG_X_RESOLUTION));
	        System.out.println("Finished running testResolution()");
	    }

	    //@Test
	    //try {
	    //	ed.testGeolocation();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testGeoLocation()
	    public void testGeoLocation() throws IOException, MetadataException
	    {
	    	ExifReaderTest er = new ExifReaderTest();
	        Metadata metadata = er.processBytes("C:/metadata/metadata-extractor-master/Tests/Data/withExifAndIptc.jpg.app1.0");

	        GpsDirectory gpsDirectory = metadata.getFirstDirectoryOfType(GpsDirectory.class);
	        assertNotNull(gpsDirectory);
	        GeoLocation geoLocation = gpsDirectory.getGeoLocation();
	        assertEquals(54.989666666666665, geoLocation.getLatitude(), 0.001);
	        assertEquals(-1.9141666666666666, geoLocation.getLongitude(), 0.001);
	        System.out.println("Finished running testGeoLocation()");
	    }

	    //@Test
	    //try {
	    //	ed.testGpsDate();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testGpsDate()
	    public void testGpsDate() throws IOException, MetadataException
	    {
	    	ExifReaderTest er = new ExifReaderTest();
	        Metadata metadata = er.processBytes("C:/metadata/metadata-extractor-master/Tests/Data/withPanasonicFaces.jpg.app1");

	        GpsDirectory gpsDirectory = metadata.getFirstDirectoryOfType(GpsDirectory.class);
	        assertNotNull(gpsDirectory);
	        assertEquals("2010:06:24", gpsDirectory.getString(GpsDirectory.TAG_DATE_STAMP));
	        assertEquals("10/1 17/1 21/1", gpsDirectory.getString(GpsDirectory.TAG_TIME_STAMP));
	        assertEquals(1277374641000L, gpsDirectory.getGpsDate().getTime());
	        System.out.println("Finished running testGpsDate()");
	    }
	}


	
	/**
	 * Unit tests for {@link ExifIFD0Descriptor}.
	 *
	 * @author Drew Noakes https://drewnoakes.com
	 */
	public class ExifIFD0DescriptorTest
	{
		//MetadataExtractorTest me = new MetadataExtractorTest();
    	//ExifIFD0DescriptorTest ed = me.new ExifIFD0DescriptorTest();
		public ExifIFD0DescriptorTest() {
			
		}
	    //@Test
		//try {
	    //	ed.testXResolutionDescription();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
		//Finished running testXResolutionDescription()
	    public void testXResolutionDescription() throws Exception
	    {
	        ExifIFD0Directory directory = new ExifIFD0Directory();
	        directory.setRational(ExifIFD0Directory.TAG_X_RESOLUTION, new Rational(72, 1));
	        // 2 is for 'Inch'
	        directory.setInt(ExifIFD0Directory.TAG_RESOLUTION_UNIT, 2);
	        ExifIFD0Descriptor descriptor = new ExifIFD0Descriptor(directory);
	        assertEquals("72 dots per inch", descriptor.getDescription(ExifIFD0Directory.TAG_X_RESOLUTION));
	        System.out.println("Finished running testXResolutionDescription()");
	    }

	    //@Test
	    //try {
	    //	ed.testYResolutionDescription();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testYResolutionDescription()
	    public void testYResolutionDescription() throws Exception
	    {
	        ExifIFD0Directory directory = new ExifIFD0Directory();
	        directory.setRational(ExifIFD0Directory.TAG_Y_RESOLUTION, new Rational(50, 1));
	        // 3 is for 'cm'
	        directory.setInt(ExifIFD0Directory.TAG_RESOLUTION_UNIT, 3);
	        ExifIFD0Descriptor descriptor = new ExifIFD0Descriptor(directory);
	        assertEquals("50 dots per cm", descriptor.getDescription(ExifIFD0Directory.TAG_Y_RESOLUTION));
	        System.out.println("Finished running testYResolutionDescription()");
	    }

	    //@Test
	    //try {
	    //	ed.testWindowsXpFields();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testWindowsXpFields()
	    public void testWindowsXpFields() throws Exception
	    {
	    	ExifReaderTest er = new ExifReaderTest();
	        ExifIFD0Directory directory = er.processBytes("C:/metadata/metadata-extractor-master/Tests/Data/windowsXpFields.jpg.app1", ExifIFD0Directory.class);

	        assertEquals("Testing artist\0", directory.getString(ExifIFD0Directory.TAG_WIN_AUTHOR, "UTF-16LE"));
	        assertEquals("Testing comments\0", directory.getString(ExifIFD0Directory.TAG_WIN_COMMENT, "UTF-16LE"));
	        assertEquals("Testing keywords\0", directory.getString(ExifIFD0Directory.TAG_WIN_KEYWORDS, "UTF-16LE"));
	        assertEquals("Testing subject\0", directory.getString(ExifIFD0Directory.TAG_WIN_SUBJECT, "UTF-16LE"));
	        assertEquals("Testing title\0", directory.getString(ExifIFD0Directory.TAG_WIN_TITLE, "UTF-16LE"));

	        ExifIFD0Descriptor descriptor = new ExifIFD0Descriptor(directory);
	        assertEquals("Testing artist", descriptor.getDescription(ExifIFD0Directory.TAG_WIN_AUTHOR));
	        assertEquals("Testing comments", descriptor.getDescription(ExifIFD0Directory.TAG_WIN_COMMENT));
	        assertEquals("Testing keywords", descriptor.getDescription(ExifIFD0Directory.TAG_WIN_KEYWORDS));
	        assertEquals("Testing subject", descriptor.getDescription(ExifIFD0Directory.TAG_WIN_SUBJECT));
	        assertEquals("Testing title", descriptor.getDescription(ExifIFD0Directory.TAG_WIN_TITLE));
	        System.out.println("Finished running testWindowsXpFields()");
	    }
	}

	/**
	 * Unit tests for {@link ExifInteropDescriptor}.
	 *
	 * @author Drew Noakes https://drewnoakes.com
	 */
	public class ExifInteropDescriptorTest
	{
		//MetadataExtractorTest me = new MetadataExtractorTest();
    	//ExifInteropDescriptorTest ei = me.new ExifInteropDescriptorTest();
		public ExifInteropDescriptorTest() {
			
		}
	    //@Test
		//try {
	    //	ei.testGetInteropVersionDescription();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
		//Finished running testGetInteropVersionDescription()
	    public void testGetInteropVersionDescription() throws Exception
	    {
	        ExifInteropDirectory directory = new ExifInteropDirectory();
	        directory.setIntArray(ExifInteropDirectory.TAG_INTEROP_VERSION, new int[]{0, 1, 0, 0});
	        ExifInteropDescriptor descriptor = new ExifInteropDescriptor(directory);
	        assertEquals("1.00", descriptor.getDescription(ExifInteropDirectory.TAG_INTEROP_VERSION));
	        assertEquals("1.00", descriptor.getInteropVersionDescription());
	        System.out.println("Finished running testGetInteropVersionDescription()");
	    }

	    //@Test
	    //try {
	    //	ei.testGetInteropIndexDescription();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testGetIndexVersionDescription()
	    public void testGetInteropIndexDescription() throws Exception
	    {
	        ExifInteropDirectory directory = new ExifInteropDirectory();
	        directory.setString(ExifInteropDirectory.TAG_INTEROP_INDEX, "R98");
	        ExifInteropDescriptor descriptor = new ExifInteropDescriptor(directory);
	        assertEquals("Recommended Exif Interoperability Rules (ExifR98)", descriptor.getDescription(ExifInteropDirectory.TAG_INTEROP_INDEX));
	        assertEquals("Recommended Exif Interoperability Rules (ExifR98)", descriptor.getInteropIndexDescription());
	        System.out.println("Finished running testGetIndexVersionDescription()");
	    }
	}
	
	/**
	 * JUnit test case for class ExifSubIFDDescriptor.
	 *
	 * @author Drew Noakes https://drewnoakes.com
	 */
	public class ExifSubIFDDescriptorTest
	{
		//MetadataExtractorTest me = new MetadataExtractorTest();
		//ExifSubIFDDescriptorTest ei = me.new ExifSubIFDDescriptorTest();
	    //@Test
		//try {
	    //	ei.testUserCommentDescription_EmptyEncoding();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
		//Finished running testUserCommentDescription_EmptyEncoding()
	    public void testUserCommentDescription_EmptyEncoding() throws Exception
	    {
	        byte[] commentBytes = "\0\0\0\0\0\0\0\0This is a comment".getBytes();
	        ExifSubIFDDirectory directory = new ExifSubIFDDirectory();
	        directory.setByteArray(ExifSubIFDDirectory.TAG_USER_COMMENT, commentBytes);
	        ExifSubIFDDescriptor descriptor = new ExifSubIFDDescriptor(directory);
	        assertEquals("This is a comment", descriptor.getDescription(ExifSubIFDDirectory.TAG_USER_COMMENT));
	        System.out.println("Finished running testUserCommentDescription_EmptyEncoding()");
	    }

	    //@Test
	    //try {
	    //	ei.testUserCommentDescription_AsciiHeaderAsciiEncoding();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testUserCommentDescription_AsciiHeaderAsciiEncoding()
	    public void testUserCommentDescription_AsciiHeaderAsciiEncoding() throws Exception
	    {
	        byte[] commentBytes = "ASCII\0\0This is a comment".getBytes();
	        ExifSubIFDDirectory directory = new ExifSubIFDDirectory();
	        directory.setByteArray(ExifSubIFDDirectory.TAG_USER_COMMENT, commentBytes);
	        ExifSubIFDDescriptor descriptor = new ExifSubIFDDescriptor(directory);
	        assertEquals("This is a comment", descriptor.getDescription(ExifSubIFDDirectory.TAG_USER_COMMENT));
	        System.out.println("Finished running testUserCommentDescription_AsciiHeaderAsciiEncoding()");
	    }

	    //@Test
	    //try {
	    //	ei.testUserCommentDescription_BlankAscii();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testUserCommentDescription_BlankAscii()
	    public void testUserCommentDescription_BlankAscii() throws Exception
	    {
	        byte[] commentBytes = "ASCII\0\0\0          ".getBytes();
	        ExifSubIFDDirectory directory = new ExifSubIFDDirectory();
	        directory.setByteArray(ExifSubIFDDirectory.TAG_USER_COMMENT, commentBytes);
	        ExifSubIFDDescriptor descriptor = new ExifSubIFDDescriptor(directory);
	        assertEquals("", descriptor.getDescription(ExifSubIFDDirectory.TAG_USER_COMMENT));
	        System.out.println("Finished running testUserCommentDescription_BlankAscii()");
	    }

	    //@Test
	    //try {
	    //	ei.testUserCommentDescription_ZeroLengthAscii1();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testUserCommentDescription_ZeroLengthAscii1()
	    public void testUserCommentDescription_ZeroLengthAscii1() throws Exception
	    {
	        // the 10-byte encoding region is only partially full
	        byte[] commentBytes = "ASCII\0\0\0".getBytes();
	        ExifSubIFDDirectory directory = new ExifSubIFDDirectory();
	        directory.setByteArray(ExifSubIFDDirectory.TAG_USER_COMMENT, commentBytes);
	        ExifSubIFDDescriptor descriptor = new ExifSubIFDDescriptor(directory);
	        assertEquals("ASCII", descriptor.getDescription(ExifSubIFDDirectory.TAG_USER_COMMENT));
	        System.out.println("Finished running testUserCommentDescription_ZeroLengthAscii1()");
	    }

	    //@Test
	    //try {
	    //	ei.testUserCommentDescription_ZeroLengthAscii2();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testUserCommentDescription_ZeroLengthAscii2()
	    public void testUserCommentDescription_ZeroLengthAscii2() throws Exception
	    {
	        // fill the 10-byte encoding region
	        byte[] commentBytes = "ASCII\0\0\0\0\0".getBytes();
	        ExifSubIFDDirectory directory = new ExifSubIFDDirectory();
	        directory.setByteArray(ExifSubIFDDirectory.TAG_USER_COMMENT, commentBytes);
	        ExifSubIFDDescriptor descriptor = new ExifSubIFDDescriptor(directory);
	        assertEquals("", descriptor.getDescription(ExifSubIFDDirectory.TAG_USER_COMMENT));
	        System.out.println("Finished running testUserCommentDescription_ZeroLengthAscii2()");
	    }

	    //@Test
	    //try {
	    //	ei.testUnicodeComment_ActualBytes();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testUnicodeComment_ActualBytes()
	    public void testUnicodeComment_ActualBytes() throws Exception
	    {
	        byte[] commentBytes = new byte[] { 85, 78, 73, 67, 79, 68, 69, 0, 84, 0, 104, 0, 105, 0, 115, 0, 32, 0, 109, 0, 97, 0, 114, 0, 109, 0, 111, 0, 116, 0, 32, 0, 105, 0, 115, 0, 32, 0, 103, 0, 101, 0, 116, 0, 116, 0, 105, 0, 110, 0, 103, 0, 32, 0, 99, 0, 108, 0, 111, 0, 115, 0, 101, 0, 46, 0, 46, 0, 46, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0, 32, 0 };
	        ExifSubIFDDirectory directory = new ExifSubIFDDirectory();
	        directory.setByteArray(ExifSubIFDDirectory.TAG_USER_COMMENT, commentBytes);
	        ExifSubIFDDescriptor descriptor = new ExifSubIFDDescriptor(directory);
	        assertEquals("This marmot is getting close...", descriptor.getDescription(ExifSubIFDDirectory.TAG_USER_COMMENT));
	        System.out.println("Finished running testUnicodeComment_ActualBytes()");
	    }

	    //@Test
	    //try {
	    //	ei.testUnicodeComment_Ascii();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testUnicodeComment_Ascii()
	    public void testUnicodeComment_Ascii() throws Exception
	    {
	        byte[] commentBytes = new byte[] { 65, 83, 67, 73, 73, 0, 0, 0, 73, 32, 97, 109, 32, 97, 32, 99, 111, 109, 109, 101, 110, 116, 46, 32, 89, 101, 121, 46, 0 };
	        ExifSubIFDDirectory directory = new ExifSubIFDDirectory();
	        directory.setByteArray(ExifSubIFDDirectory.TAG_USER_COMMENT, commentBytes);
	        ExifSubIFDDescriptor descriptor = new ExifSubIFDDescriptor(directory);
	        assertEquals("I am a comment. Yey.", descriptor.getDescription(ExifSubIFDDirectory.TAG_USER_COMMENT));
	        System.out.println("Finished running testUnicodeComment_Ascii()");
	    }
	}


}