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
import static org.junit.Assert.assertNull;
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

	/**
	 * JUnit test case for class ExifThumbnailDescriptor.
	 *
	 * @author Drew Noakes https://drewnoakes.com
	 */
	public class ExifThumbnailDescriptorTest
	{
		//MetadataExtractorTest me = new MetadataExtractorTest();
	    //ExifThumbnailDescriptorTest ed = me.new ExifThumbnailDescriptorTest();
	    //@Test
		//try {
	    //	ed.testGetYCbCrSubsamplingDescription();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
		//Finished running testGetYCbCrSubsamplingDescription()
	    public void testGetYCbCrSubsamplingDescription() throws Exception
	    {
	        ExifThumbnailDirectory directory = new ExifThumbnailDirectory();
	        directory.setIntArray(ExifThumbnailDirectory.TAG_YCBCR_SUBSAMPLING, new int[]{2, 1});

	        ExifThumbnailDescriptor descriptor = new ExifThumbnailDescriptor(directory);
	        assertEquals("YCbCr4:2:2", descriptor.getDescription(ExifThumbnailDirectory.TAG_YCBCR_SUBSAMPLING));
	        assertEquals("YCbCr4:2:2", descriptor.getYCbCrSubsamplingDescription());

	        directory.setIntArray(ExifThumbnailDirectory.TAG_YCBCR_SUBSAMPLING, new int[]{2, 2});

	        assertEquals("YCbCr4:2:0", descriptor.getDescription(ExifThumbnailDirectory.TAG_YCBCR_SUBSAMPLING));
	        assertEquals("YCbCr4:2:0", descriptor.getYCbCrSubsamplingDescription());
	        System.out.println("Finished running testGetYCbCrSubsamplingDescription()");
	    }
	}
	
	/**
	 * @author Drew Noakes https://drewnoakes.com
	 */
	public class NikonType1MakernoteTest
	{
		//MetadataExtractorTest me = new MetadataExtractorTest();
		// NikonType1MakernoteTest nm = me.new NikonType1MakernoteTest();
		// try {
	    //    nm.setUp();
	    // }
		// catch(Exception) {
		//    e.printStackTrace();
		//}
	    private NikonType1MakernoteDirectory _nikonDirectory;
	    private ExifIFD0Directory _exifIFD0Directory;
	    private ExifSubIFDDirectory _exifSubIFDDirectory;
	    private ExifThumbnailDirectory _thumbDirectory;
	    public NikonType1MakernoteTest() {
	  
	    }

	    /*
	        [Interoperability] Interoperability Index = Recommended Exif Interoperability Rules (ExifR98)
	        [Interoperability] Interoperability Version = 1.00
	        [Jpeg] Data Precision = 8 bits
	        [Jpeg] Image Width = 600 pixels
	        [Jpeg] Image Height = 800 pixels
	        [Jpeg] Number of Components = 3
	        [Jpeg] Component 1 = Y component: Quantization table 0, Sampling factors 1 horiz/1 vert
	        [Jpeg] Component 2 = Cb component: Quantization table 1, Sampling factors 1 horiz/1 vert
	        [Jpeg] Component 3 = Cr component: Quantization table 1, Sampling factors 1 horiz/1 vert
	    */

	    //@Before
	    public void setUp() throws Exception
	    {
	    	ExifReaderTest er = new ExifReaderTest();
	        Metadata metadata = er.processBytes("C:/metadata/metadata-extractor-master/Tests/Data/nikonMakernoteType1.jpg.app1");



	        _nikonDirectory = metadata.getFirstDirectoryOfType(NikonType1MakernoteDirectory.class);
	        _exifSubIFDDirectory = metadata.getFirstDirectoryOfType(ExifSubIFDDirectory.class);
	        _exifIFD0Directory = metadata.getFirstDirectoryOfType(ExifIFD0Directory.class);
	        _thumbDirectory = metadata.getFirstDirectoryOfType(ExifThumbnailDirectory.class);

	    }

	    /*
	        [Nikon Makernote] Makernote Unknown 1 = 08.00
	        [Nikon Makernote] Quality = Unknown (12)
	        [Nikon Makernote] Color Mode = Color
	        [Nikon Makernote] Image Adjustment = Contrast +
	        [Nikon Makernote] CCD Sensitivity = ISO80
	        [Nikon Makernote] White Balance = Auto
	        [Nikon Makernote] Focus = 0
	        [Nikon Makernote] Makernote Unknown 2 =
	        [Nikon Makernote] Digital Zoom = No digital zoom
	        [Nikon Makernote] Fisheye Converter = None
	        [Nikon Makernote] Makernote Unknown 3 = 0 0 16777216 0 2685774096 0 34833 6931 16178 4372 4372 3322676767 3373084416 15112 0 0 1151495 252903424 17 0 0 844038208 55184128 218129428 1476410198 370540566 4044363286 16711749 204629079 1729
	    */
	    
	    //@Test
	    //try {
	    //	nm.testNikonMakernote_MatchesKnownValues();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testNikonMakernote_MatchesKnownValues()
	    public void testNikonMakernote_MatchesKnownValues() throws Exception
	    {
	        assertTrue(_nikonDirectory.getTagCount() > 0);
	        assertEquals(8, _nikonDirectory.getDouble(NikonType1MakernoteDirectory.TAG_UNKNOWN_1), 0.0001);
	        assertEquals(12, _nikonDirectory.getInt(NikonType1MakernoteDirectory.TAG_QUALITY));
	        assertEquals(1, _nikonDirectory.getInt(NikonType1MakernoteDirectory.TAG_COLOR_MODE));
	        assertEquals(3, _nikonDirectory.getInt(NikonType1MakernoteDirectory.TAG_IMAGE_ADJUSTMENT));
	        assertEquals(0, _nikonDirectory.getInt(NikonType1MakernoteDirectory.TAG_CCD_SENSITIVITY));
	        assertEquals(0, _nikonDirectory.getInt(NikonType1MakernoteDirectory.TAG_WHITE_BALANCE));
	        assertEquals(0, _nikonDirectory.getInt(NikonType1MakernoteDirectory.TAG_FOCUS));
	        assertEquals("", _nikonDirectory.getString(NikonType1MakernoteDirectory.TAG_UNKNOWN_2));
	        assertEquals(0, _nikonDirectory.getDouble(NikonType1MakernoteDirectory.TAG_DIGITAL_ZOOM), 0.0001);
	        assertEquals(0, _nikonDirectory.getInt(NikonType1MakernoteDirectory.TAG_CONVERTER));
	        long[] unknown3 = (long[])_nikonDirectory.getObject(NikonType1MakernoteDirectory.TAG_UNKNOWN_3);
	        long[] expected = new long[] { 0, 0, 16777216, 0, 2685774096L, 0, 34833, 6931, 16178, 4372, 4372, 3322676767L, 3373084416L, 15112, 0, 0, 1151495, 252903424, 17, 0, 0, 844038208, 55184128, 218129428, 1476410198, 370540566, 4044363286L, 16711749, 204629079, 1729 };
	        assertNotNull(unknown3);
	        assertEquals(expected.length, unknown3.length);
	        for (int i = 0; i<expected.length; i++) {
	            assertEquals(expected[i], unknown3[i]);
	        }
	        System.out.println("Finished running testNikonMakernote_MatchesKnownValues()");
	    }

	    /*
	        [Exif] Image Description =
	        [Exif] Make = NIKON
	        [Exif] Model = E950
	        [Exif] Orientation = top, left side
	        [Exif] X Resolution = 300 dots per inch
	        [Exif] Y Resolution = 300 dots per inch
	        [Exif] Resolution Unit = Inch
	        [Exif] Software = v981-79
	        [Exif] Date/Time = 2001:04:06 11:51:40
	        [Exif] YCbCr Positioning = Datum point
	        [Exif] Exposure Time = 1/77 sec
	        [Exif] F-Number = F5.5
	        [Exif] Exposure Program = Program normal
	        [Exif] ISO Speed Ratings = 80
	        [Exif] Exif Version = 2.10
	        [Exif] Date/Time Original = 2001:04:06 11:51:40
	        [Exif] Date/Time Digitized = 2001:04:06 11:51:40
	        [Exif] Components Configuration = YCbCr
	        [Exif] Compressed Bits Per Pixel = 4 bits/pixel
	        [Exif] Exposure Bias Value = 0
	        [Exif] Max Aperture Value = F2.5
	        [Exif] Metering Mode = Multi-segment
	        [Exif] Light Source = Unknown
	        [Exif] Flash = No flash fired
	        [Exif] Focal Length = 12.8 mm
	        [Exif] User Comment =
	        [Exif] FlashPix Version = 1.00
	        [Exif] Color Space = sRGB
	        [Exif] Exif Image Width = 1600 pixels
	        [Exif] Exif Image Height = 1200 pixels
	        [Exif] File Source = Digital Still Camera (DSC)
	        [Exif] Scene Type = Directly photographed image
	        [Exif] Compression = JPEG compression
	        [Exif] Thumbnail Offset = 2036 bytes
	        [Exif] Thumbnail Length = 4662 bytes
	        [Exif] Thumbnail Data = [4662 bytes of thumbnail data]
	    */
	    //@Test
	    //try {
	    //	nm.testExifDirectory_MatchesKnownValues();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testExifDirectory_MatchesKnownValues()
	    public void testExifDirectory_MatchesKnownValues() throws Exception
	    {
	        assertEquals("          ", _exifIFD0Directory.getString(ExifIFD0Directory.TAG_IMAGE_DESCRIPTION));
	        assertEquals("NIKON", _exifIFD0Directory.getString(ExifIFD0Directory.TAG_MAKE));
	        assertEquals("E950", _exifIFD0Directory.getString(ExifIFD0Directory.TAG_MODEL));
	        assertEquals(1, _exifIFD0Directory.getInt(ExifIFD0Directory.TAG_ORIENTATION));
	        assertEquals(300, _exifIFD0Directory.getDouble(ExifIFD0Directory.TAG_X_RESOLUTION), 0.001);
	        assertEquals(300, _exifIFD0Directory.getDouble(ExifIFD0Directory.TAG_Y_RESOLUTION), 0.001);
	        assertEquals(2, _exifIFD0Directory.getInt(ExifIFD0Directory.TAG_RESOLUTION_UNIT));
	        assertEquals("v981-79", _exifIFD0Directory.getString(ExifIFD0Directory.TAG_SOFTWARE));
	        assertEquals("2001:04:06 11:51:40", _exifIFD0Directory.getString(ExifIFD0Directory.TAG_DATETIME));
	        assertEquals(2, _exifIFD0Directory.getInt(ExifIFD0Directory.TAG_YCBCR_POSITIONING));

	        assertEquals(new Rational(1, 77), _exifSubIFDDirectory.getRational(ExifSubIFDDirectory.TAG_EXPOSURE_TIME));
	        assertEquals(5.5, _exifSubIFDDirectory.getDouble(ExifSubIFDDirectory.TAG_FNUMBER), 0.001);
	        assertEquals(2, _exifSubIFDDirectory.getInt(ExifSubIFDDirectory.TAG_EXPOSURE_PROGRAM));
	        assertEquals(80, _exifSubIFDDirectory.getInt(ExifSubIFDDirectory.TAG_ISO_EQUIVALENT));
	        assertEquals("48 50 49 48", _exifSubIFDDirectory.getString(ExifSubIFDDirectory.TAG_EXIF_VERSION));
	        assertEquals("2001:04:06 11:51:40", _exifSubIFDDirectory.getString(ExifSubIFDDirectory.TAG_DATETIME_DIGITIZED));
	        assertEquals("2001:04:06 11:51:40", _exifSubIFDDirectory.getString(ExifSubIFDDirectory.TAG_DATETIME_ORIGINAL));
	        assertEquals("1 2 3 0", _exifSubIFDDirectory.getString(ExifSubIFDDirectory.TAG_COMPONENTS_CONFIGURATION));
	        assertEquals(4, _exifSubIFDDirectory.getInt(ExifSubIFDDirectory.TAG_COMPRESSED_AVERAGE_BITS_PER_PIXEL));
	        assertEquals(0, _exifSubIFDDirectory.getInt(ExifSubIFDDirectory.TAG_EXPOSURE_BIAS));
	        // this 2.6 *apex*, which is F2.5
	        assertEquals(2.6, _exifSubIFDDirectory.getDouble(ExifSubIFDDirectory.TAG_MAX_APERTURE), 0.001);
	        assertEquals(5, _exifSubIFDDirectory.getInt(ExifSubIFDDirectory.TAG_METERING_MODE));
	        assertEquals(0, _exifSubIFDDirectory.getInt(ExifSubIFDDirectory.TAG_WHITE_BALANCE));
	        assertEquals(0, _exifSubIFDDirectory.getInt(ExifSubIFDDirectory.TAG_FLASH));
	        assertEquals(12.8, _exifSubIFDDirectory.getDouble(ExifSubIFDDirectory.TAG_FOCAL_LENGTH), 0.001);
	        assertEquals("0 0 0 0 0 0 0 0 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32", _exifSubIFDDirectory.getString(ExifSubIFDDirectory.TAG_USER_COMMENT));
	        assertEquals("48 49 48 48", _exifSubIFDDirectory.getString(ExifSubIFDDirectory.TAG_FLASHPIX_VERSION));
	        assertEquals(1, _exifSubIFDDirectory.getInt(ExifSubIFDDirectory.TAG_COLOR_SPACE));
	        assertEquals(1600, _exifSubIFDDirectory.getInt(ExifSubIFDDirectory.TAG_EXIF_IMAGE_WIDTH));
	        assertEquals(1200, _exifSubIFDDirectory.getInt(ExifSubIFDDirectory.TAG_EXIF_IMAGE_HEIGHT));
	        assertEquals(3, _exifSubIFDDirectory.getInt(ExifSubIFDDirectory.TAG_FILE_SOURCE));
	        assertEquals(1, _exifSubIFDDirectory.getInt(ExifSubIFDDirectory.TAG_SCENE_TYPE));

	        assertEquals(6, _thumbDirectory.getInt(ExifThumbnailDirectory.TAG_COMPRESSION));
	        assertEquals(2036, _thumbDirectory.getInt(ExifThumbnailDirectory.TAG_THUMBNAIL_OFFSET));
	        assertEquals(4662, _thumbDirectory.getInt(ExifThumbnailDirectory.TAG_THUMBNAIL_LENGTH));
	        System.out.println("Finished running testExifDirectory_MatchesKnownValues()");
	    }
	}

	public class NikonType2MakernoteTest1
	{
		//MetadataExtractorTest me = new MetadataExtractorTest();
		// NikonType2MakernoteTest1 nm = me.new NikonType2MakernoteTest1();
		// try {
	    //    nm.setUp();
	    // }
		// catch(Exception) {
		//    e.printStackTrace();
		//}
	    private NikonType2MakernoteDirectory _nikonDirectory;
	    private NikonType2MakernoteDescriptor _descriptor;
	    public NikonType2MakernoteTest1() {
	    	
	    }

	    //@Before
	    public void setUp() throws Exception
	    {
	        Locale.setDefault(new Locale("en", "GB"));

	        ExifReaderTest er = new ExifReaderTest();
	        _nikonDirectory = er.processBytes("C:/metadata/metadata-extractor-master/Tests/Data/nikonMakernoteType2a.jpg.app1", NikonType2MakernoteDirectory.class);

	        assertNotNull(_nikonDirectory);

	        _descriptor = new NikonType2MakernoteDescriptor(_nikonDirectory);
	    }

	    /*
	        [Nikon Makernote] Firmware Version = 0200
	        [Nikon Makernote] ISO = 0 320
	        [Nikon Makernote] File Format = FINE
	        [Nikon Makernote] White Balance = FLASH
	        [Nikon Makernote] Sharpening = AUTO
	        [Nikon Makernote] AF Type = AF-C
	        [Nikon Makernote] Unknown 17 = NORMAL
	        [Nikon Makernote] Unknown 18 =
	        [Nikon Makernote] White Balance Fine = 0
	        [Nikon Makernote] Unknown 01 =
	        [Nikon Makernote] Unknown 02 =
	        [Nikon Makernote] Unknown 03 = 914
	        [Nikon Makernote] Unknown 19 =
	        [Nikon Makernote] ISO = 0 320
	        [Nikon Makernote] Tone Compensation = AUTO
	        [Nikon Makernote] Unknown 04 = 6
	        [Nikon Makernote] Lens Focal/Max-FStop pairs = 240/10 850/10 35/10 45/10
	        [Nikon Makernote] Unknown 05 = 0
	        [Nikon Makernote] Unknown 06 = 
	        [Nikon Makernote] Unknown 07 = 1
	        [Nikon Makernote] Unknown 20 = 0
	        [Nikon Makernote] Unknown 08 = @
	        [Nikon Makernote] Colour Mode = MODE1
	        [Nikon Makernote] Unknown 10 = NATURAL
	        [Nikon Makernote] Unknown 11 = 0100
	        
	

	        


	        
	-
	        [Nikon Makernote] Camera Hue = 0
	        [Nikon Makernote] Noise Reduction = OFF
	        [Nikon Makernote] Unknown 12 = 0100

	        [Nikon Makernote] Unknown 13 = 0100{t@7b,4x,D"Y
	        [Nikon Makernote] Unknown 15 = 78/10 78/10
	    */
	    //@Test
	    //try {
	    //	nm.testNikonMakernote_MatchesKnownValues();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testNikonMakernote_MatchesKnownValues()
	    public void testNikonMakernote_MatchesKnownValues() throws Exception
	    {
	        assertEquals("48 50 48 48", _nikonDirectory.getString(NikonType2MakernoteDirectory.TAG_FIRMWARE_VERSION));
	        assertEquals("0 320", _nikonDirectory.getString(NikonType2MakernoteDirectory.TAG_ISO_1));
	        assertEquals("0 320", _nikonDirectory.getString(NikonType2MakernoteDirectory.TAG_ISO_REQUESTED));
	        assertEquals("FLASH       ", _nikonDirectory.getString(NikonType2MakernoteDirectory.TAG_CAMERA_WHITE_BALANCE));
	        assertEquals("AUTO  ", _nikonDirectory.getString(NikonType2MakernoteDirectory.TAG_CAMERA_SHARPENING));
	        assertEquals("AF-C  ", _nikonDirectory.getString(NikonType2MakernoteDirectory.TAG_AF_TYPE));
	        assertEquals("NORMAL      ", _nikonDirectory.getString(NikonType2MakernoteDirectory.TAG_FLASH_SYNC_MODE));
	        assertEquals("0", _nikonDirectory.getString(NikonType2MakernoteDirectory.TAG_CAMERA_WHITE_BALANCE_FINE));
	        assertEquals("914", _nikonDirectory.getString(NikonType2MakernoteDirectory.TAG_PREVIEW_IFD));
	        assertEquals("AUTO    ", _nikonDirectory.getString(NikonType2MakernoteDirectory.TAG_CAMERA_TONE_COMPENSATION));
	        assertEquals("6", _nikonDirectory.getString(NikonType2MakernoteDirectory.TAG_LENS_TYPE));
	        assertEquals("240/10 850/10 35/10 45/10", _nikonDirectory.getString(NikonType2MakernoteDirectory.TAG_LENS));
	        assertEquals("0", _nikonDirectory.getString(NikonType2MakernoteDirectory.TAG_FLASH_USED));
	        assertEquals("1", _nikonDirectory.getString(NikonType2MakernoteDirectory.TAG_SHOOTING_MODE));
	        assertEquals("0", _nikonDirectory.getString(NikonType2MakernoteDirectory.TAG_UNKNOWN_20));
	        assertEquals("MODE1   ", _nikonDirectory.getString(NikonType2MakernoteDirectory.TAG_CAMERA_COLOR_MODE));
	        assertEquals("NATURAL    ", _nikonDirectory.getString(NikonType2MakernoteDirectory.TAG_LIGHT_SOURCE));
	        assertEquals("0", _nikonDirectory.getString(NikonType2MakernoteDirectory.TAG_CAMERA_HUE_ADJUSTMENT));
	        assertEquals("OFF ", _nikonDirectory.getString(NikonType2MakernoteDirectory.TAG_NOISE_REDUCTION));
	        assertEquals("78/10 78/10", _nikonDirectory.getString(NikonType2MakernoteDirectory.TAG_SENSOR_PIXEL_SIZE));
	        System.out.println("Finished running testNikonMakernote_MatchesKnownValues()");
	    }

	    //@Test
	    //try {
	    //	nm.testGetLensDescription();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testGetLensDescription()
	    public void testGetLensDescription() throws MetadataException
	    {
	        assertEquals("24-85mm f/3.5-4.5", _descriptor.getDescription(NikonType2MakernoteDirectory.TAG_LENS));
	        assertEquals("24-85mm f/3.5-4.5", _descriptor.getLensDescription());
	        System.out.println("Finished running testGetLensDescription()");
	    }

	   // @Test
	    //try {
	    //	nm.testGetHueAdjustmentDescription();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testGetHueAdjustmentDescription()
	    public void testGetHueAdjustmentDescription() throws MetadataException
	    {
	        assertEquals("0 degrees", _descriptor.getDescription(NikonType2MakernoteDirectory.TAG_CAMERA_HUE_ADJUSTMENT));
	        assertEquals("0 degrees", _descriptor.getHueAdjustmentDescription());
	        System.out.println("Finished running testGetHueAdjustmentDescription()");
	    }

	    //@Test
	    //try {
	    //	nm.testGetColorModeDescription();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testGetColorModeDescription()
	    public void testGetColorModeDescription() throws Exception
	    {
	        assertEquals("Mode I (sRGB)", _descriptor.getDescription(NikonType2MakernoteDirectory.TAG_CAMERA_COLOR_MODE));
	        assertEquals("Mode I (sRGB)", _descriptor.getColorModeDescription());
	        System.out.println("Finished running testGetColorModeDescription()");
	    }

	    //@Test
	    //try {
	    //	nm.testGetAutoFlashCompensationDescription();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testGetAutoFlashCompensationDescription()
	    public void testGetAutoFlashCompensationDescription() throws Exception
	    {
	        NikonType2MakernoteDirectory directory = new NikonType2MakernoteDirectory();
	        NikonType2MakernoteDescriptor descriptor = new NikonType2MakernoteDescriptor(directory);

	        // no entry exists
	        assertNull(descriptor.getAutoFlashCompensationDescription());

	        directory.setByteArray(NikonType2MakernoteDirectory.TAG_AUTO_FLASH_COMPENSATION, new byte[] { 0x06, 0x01, 0x06 });
	        assertEquals("1 EV", descriptor.getAutoFlashCompensationDescription());

	        directory.setByteArray(NikonType2MakernoteDirectory.TAG_AUTO_FLASH_COMPENSATION, new byte[] { 0x04, 0x01, 0x06 });
	        assertEquals("0.67 EV", descriptor.getAutoFlashCompensationDescription());

	        directory.setByteArray(NikonType2MakernoteDirectory.TAG_AUTO_FLASH_COMPENSATION, new byte[] { 0x02, 0x01, 0x06 });
	        assertEquals("0.33 EV", descriptor.getAutoFlashCompensationDescription());

	        directory.setByteArray(NikonType2MakernoteDirectory.TAG_AUTO_FLASH_COMPENSATION, new byte[] { (byte)0xFE, 0x01, 0x06 });
	        assertEquals("-0.33 EV", descriptor.getAutoFlashCompensationDescription());
	        System.out.println("Finished running testGetAutoFlashCompensationDescription()");
	    }
	}
	
	/**
	 * @author Drew Noakes https://drewnoakes.com
	 */
	public class NikonType2MakernoteTest2
	{
		//MetadataExtractorTest me = new MetadataExtractorTest();
		// NikonType2MakernoteTest2 nm = me.new NikonType2MakernoteTest2();
		// try {
	    //    nm.setUp();
	    // }
		// catch(Exception) {
		//    e.printStackTrace();
		//}
		public NikonType2MakernoteTest2() {
			
		}
	    private Metadata _metadata;
	    private NikonType2MakernoteDirectory _nikonDirectory;
	    private ExifIFD0Directory _exifIFD0Directory;
	    private ExifSubIFDDirectory _exifSubIFDDirectory;
	    private ExifThumbnailDirectory _thumbDirectory;

	    //@Before
	    public void setUp() throws Exception
	    {
	        ExifReaderTest er = new ExifReaderTest();
	    	_metadata = er.processBytes("C:/metadata/metadata-extractor-master/Tests/Data/nikonMakernoteType2b.jpg.app1");

	        _nikonDirectory = _metadata.getFirstDirectoryOfType(NikonType2MakernoteDirectory.class);
	        _exifIFD0Directory = _metadata.getFirstDirectoryOfType(ExifIFD0Directory.class);
	        _exifSubIFDDirectory = _metadata.getFirstDirectoryOfType(ExifSubIFDDirectory.class);
	        _thumbDirectory = _metadata.getFirstDirectoryOfType(ExifThumbnailDirectory.class);

	        assertNotNull(_nikonDirectory);
	        assertNotNull(_exifSubIFDDirectory);
	    }

	    /*
	        [Nikon Makernote] Makernote Unknown 1 =
	        [Nikon Makernote] ISO Setting = Unknown (0 0)
	        [Nikon Makernote] Color Mode = COLOR
	        [Nikon Makernote] Quality = NORMAL
	        [Nikon Makernote] White Balance = AUTO
	        [Nikon Makernote] Image Sharpening = AUTO
	        [Nikon Makernote] Focus Mode = AF-C
	        [Nikon Makernote] Flash Setting = NORMAL
	        [Nikon Makernote] Makernote Unknown 2 = 4416/500
	        [Nikon Makernote] ISO Selection = AUTO
	        [Nikon Makernote] Unknown tag (0x0011) = 1300
	        [Nikon Makernote] Image Adjustment = AUTO
	        [Nikon Makernote] Adapter = OFF
	        [Nikon Makernote] Focus Distance = 0
	        [Nikon Makernote] Digital Zoom = No digital zoom
	        [Nikon Makernote] AF Focus Position = Unknown ()
	        [Nikon Makernote] Unknown tag (0x008f) =
	        [Nikon Makernote] Unknown tag (0x0094) = 0
	        [Nikon Makernote] Unknown tag (0x0095) = FPNR
	        [Nikon Makernote] Unknown tag (0x0e00) = PrintIM
	        [Nikon Makernote] Unknown tag (0x0e10) = 1394
	    */
	    //@Test
	    //try {
	    //	nm.testNikonMakernote_MatchesKnownValues();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testNikonMakernote_MatchesKnownValues()
	    public void testNikonMakernote_MatchesKnownValues() throws Exception
	    {
	        assertEquals("0 1 0 0", _nikonDirectory.getString(NikonType2MakernoteDirectory.TAG_FIRMWARE_VERSION));
	        assertEquals("0 0", _nikonDirectory.getString(NikonType2MakernoteDirectory.TAG_ISO_1));
	        assertEquals("COLOR", _nikonDirectory.getString(NikonType2MakernoteDirectory.TAG_COLOR_MODE));
	        assertEquals("NORMAL ", _nikonDirectory.getString(NikonType2MakernoteDirectory.TAG_QUALITY_AND_FILE_FORMAT));
	        assertEquals("AUTO        ", _nikonDirectory.getString(NikonType2MakernoteDirectory.TAG_CAMERA_WHITE_BALANCE));
	        assertEquals("AUTO  ", _nikonDirectory.getString(NikonType2MakernoteDirectory.TAG_CAMERA_SHARPENING));
	        assertEquals("AF-C  ", _nikonDirectory.getString(NikonType2MakernoteDirectory.TAG_AF_TYPE));
	        assertEquals("NORMAL      ", _nikonDirectory.getString(NikonType2MakernoteDirectory.TAG_FLASH_SYNC_MODE));
//	        assertEquals(new Rational(4416,500), _nikonDirectory.getRational(NikonType3MakernoteDirectory.TAG_UNKNOWN_2));
	        assertEquals("AUTO  ", _nikonDirectory.getString(NikonType2MakernoteDirectory.TAG_ISO_MODE));
	        assertEquals(1300, _nikonDirectory.getInt(0x0011));
	        assertEquals("AUTO         ", _nikonDirectory.getString(NikonType2MakernoteDirectory.TAG_IMAGE_ADJUSTMENT));
	        assertEquals("OFF         ", _nikonDirectory.getString(NikonType2MakernoteDirectory.TAG_ADAPTER));
	        assertEquals(0, _nikonDirectory.getInt(NikonType2MakernoteDirectory.TAG_MANUAL_FOCUS_DISTANCE));
	        assertEquals(1, _nikonDirectory.getInt(NikonType2MakernoteDirectory.TAG_DIGITAL_ZOOM));
	        assertEquals("                ", _nikonDirectory.getString(0x008f));
	        assertEquals(0, _nikonDirectory.getInt(0x0094));
	        assertEquals("FPNR", _nikonDirectory.getString(0x0095));

	        // PrintIM
	        HashMap<Integer, String> _expectedData = new HashMap<Integer, String>();
	        _expectedData.put(0x0000, "0100");
	        _expectedData.put(0x0001, "0x00160016");
	        _expectedData.put(0x0002, "0x00000001");
	        _expectedData.put(0x0003, "0x0000005e");
	        _expectedData.put(0x0007, "0x00000000");
	        _expectedData.put(0x0008, "0x00000000");
	        _expectedData.put(0x0009, "0x00000000");
	        _expectedData.put(0x000A, "0x00000000");
	        _expectedData.put(0x000B, "0x000000a6");
	        _expectedData.put(0x000C, "0x00000000");
	        _expectedData.put(0x000D, "0x00000000");
	        _expectedData.put(0x000E, "0x000000be");
	        _expectedData.put(0x0100, "0x00000005");
	        _expectedData.put(0x0101, "0x00000001");

	        PrintIMDirectory nikonPrintImDirectory = _metadata.getFirstDirectoryOfType(PrintIMDirectory.class);

	        assertNotNull(nikonPrintImDirectory);

	        assertEquals(_expectedData.size(), nikonPrintImDirectory.getTagCount());
	        for (Map.Entry<Integer, String> _expected : _expectedData.entrySet())
	        {
	            assertEquals(_expected.getValue(), nikonPrintImDirectory.getDescription(_expected.getKey()));
	        }

//	        assertEquals("80 114 105 110 116 73 77 0 48 49 48 48 0 0 13 0 1 0 22 0 22 0 2 0 1 0 0 0 3 0 94 0 0 0 7 0 0 0 0 0 8 0 0 0 0 0 9 0 0 0 0 0 10 0 0 0 0 0 11 0 166 0 0 0 12 0 0 0 0 0 13 0 0 0 0 0 14 0 190 0 0 0 0 1 5 0 0 0 1 1 1 0 0 0 9 17 0 0 16 39 0 0 11 15 0 0 16 39 0 0 151 5 0 0 16 39 0 0 176 8 0 0 16 39 0 0 1 28 0 0 16 39 0 0 94 2 0 0 16 39 0 0 139 0 0 0 16 39 0 0 203 3 0 0 16 39 0 0 229 27 0 0 16 39 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0", _nikonDirectory.getString(0x0e00));
//	        assertEquals("PrintIM", _nikonDirectory.getString(0x0e00));
	        assertEquals(1394, _nikonDirectory.getInt(0x0e10));
	        System.out.println("Finished running testNikonMakernote_MatchesKnownValues()");
	    }

	    /*
	        [Exif] Image Description =
	        [Exif] Make = NIKON
	        [Exif] Model = E995
	        [Exif] X Resolution = 300 dots per inch
	        [Exif] Y Resolution = 300 dots per inch
	        [Exif] Resolution Unit = Inch
	        [Exif] Software = E995v1.6
	        [Exif] Date/Time = 2002:08:29 17:31:40
	        [Exif] YCbCr Positioning = Center of pixel array
	        [Exif] Exposure Time = 2439024/100000000 sec
	        [Exif] F-Number = F2.6
	        [Exif] Exposure Program = Program normal
	        [Exif] ISO Speed Ratings = 100
	        [Exif] Exif Version = 2.10
	        [Exif] Date/Time Original = 2002:08:29 17:31:40
	        [Exif] Date/Time Digitized = 2002:08:29 17:31:40
	        [Exif] Components Configuration = YCbCr
	        [Exif] Exposure Bias Value = 0 EV
	        [Exif] Max Aperture Value = F1
	        [Exif] Metering Mode = Multi-segment
	        [Exif] White Balance = Unknown
	        [Exif] Flash = Flash fired
	        [Exif] Focal Length = 8.2 mm
	        [Exif] User Comment =
	        [Exif] FlashPix Version = 1.00
	        [Exif] Color Space = sRGB
	        [Exif] Exif Image Width = 2048 pixels
	        [Exif] Exif Image Height = 1536 pixels
	        [Exif] File Source = Digital Still Camera (DSC)
	        [Exif] Scene Type = Directly photographed image
	    */
	    //@Test
	    //try {
	    //	nm.testExifDirectory_MatchesKnownValues();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testExifDirectory_MatchesKnownValues()
	    public void testExifDirectory_MatchesKnownValues() throws Exception
	    {
	        assertEquals("          ", _exifIFD0Directory.getString(ExifIFD0Directory.TAG_IMAGE_DESCRIPTION));
	        assertEquals("NIKON", _exifIFD0Directory.getString(ExifIFD0Directory.TAG_MAKE));
	        assertEquals("E995", _exifIFD0Directory.getString(ExifIFD0Directory.TAG_MODEL));
	        assertEquals(300, _exifIFD0Directory.getDouble(ExifIFD0Directory.TAG_X_RESOLUTION), 0.001);
	        assertEquals(300, _exifIFD0Directory.getDouble(ExifIFD0Directory.TAG_Y_RESOLUTION), 0.001);
	        assertEquals(2, _exifIFD0Directory.getInt(ExifIFD0Directory.TAG_RESOLUTION_UNIT));
	        assertEquals("E995v1.6", _exifIFD0Directory.getString(ExifIFD0Directory.TAG_SOFTWARE));
	        assertEquals("2002:08:29 17:31:40", _exifIFD0Directory.getString(ExifIFD0Directory.TAG_DATETIME));
	        assertEquals(1, _exifIFD0Directory.getInt(ExifIFD0Directory.TAG_YCBCR_POSITIONING));

	        assertEquals(new Rational(2439024, 100000000), _exifSubIFDDirectory.getRational(ExifSubIFDDirectory.TAG_EXPOSURE_TIME));
	        assertEquals(2.6, _exifSubIFDDirectory.getDouble(ExifSubIFDDirectory.TAG_FNUMBER), 0.001);
	        assertEquals(2, _exifSubIFDDirectory.getInt(ExifSubIFDDirectory.TAG_EXPOSURE_PROGRAM));
	        assertEquals(100, _exifSubIFDDirectory.getInt(ExifSubIFDDirectory.TAG_ISO_EQUIVALENT));
	        assertEquals("48 50 49 48", _exifSubIFDDirectory.getString(ExifSubIFDDirectory.TAG_EXIF_VERSION));
	        assertEquals("2002:08:29 17:31:40", _exifSubIFDDirectory.getString(ExifSubIFDDirectory.TAG_DATETIME_DIGITIZED));
	        assertEquals("2002:08:29 17:31:40", _exifSubIFDDirectory.getString(ExifSubIFDDirectory.TAG_DATETIME_ORIGINAL));
	        assertEquals("1 2 3 0", _exifSubIFDDirectory.getString(ExifSubIFDDirectory.TAG_COMPONENTS_CONFIGURATION));
	        assertEquals(0, _exifSubIFDDirectory.getInt(ExifSubIFDDirectory.TAG_EXPOSURE_BIAS));
	        assertEquals("0", _exifSubIFDDirectory.getString(ExifSubIFDDirectory.TAG_MAX_APERTURE));
	        assertEquals(5, _exifSubIFDDirectory.getInt(ExifSubIFDDirectory.TAG_METERING_MODE));
	        assertEquals(0, _exifSubIFDDirectory.getInt(ExifSubIFDDirectory.TAG_WHITE_BALANCE));
	        assertEquals(1, _exifSubIFDDirectory.getInt(ExifSubIFDDirectory.TAG_FLASH));
	        assertEquals(8.2, _exifSubIFDDirectory.getDouble(ExifSubIFDDirectory.TAG_FOCAL_LENGTH), 0.001);
	        assertEquals("0 0 0 0 0 0 0 0 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32", _exifSubIFDDirectory.getString(ExifSubIFDDirectory.TAG_USER_COMMENT));
	        assertEquals("48 49 48 48", _exifSubIFDDirectory.getString(ExifSubIFDDirectory.TAG_FLASHPIX_VERSION));
	        assertEquals(1, _exifSubIFDDirectory.getInt(ExifSubIFDDirectory.TAG_COLOR_SPACE));
	        assertEquals(2048, _exifSubIFDDirectory.getInt(ExifSubIFDDirectory.TAG_EXIF_IMAGE_WIDTH));
	        assertEquals(1536, _exifSubIFDDirectory.getInt(ExifSubIFDDirectory.TAG_EXIF_IMAGE_HEIGHT));
	        assertEquals(3, _exifSubIFDDirectory.getInt(ExifSubIFDDirectory.TAG_FILE_SOURCE));
	        assertEquals(1, _exifSubIFDDirectory.getInt(ExifSubIFDDirectory.TAG_SCENE_TYPE));
	        System.out.println("Finished running testExifDirectory_MatchesKnownValues()");
	    }

	    /*
	        [Exif Thumbnail] Thumbnail Compression = JPEG (old-style)
	        [Exif Thumbnail] X Resolution = 72 dots per inch
	        [Exif Thumbnail] Y Resolution = 72 dots per inch
	        [Exif Thumbnail] Resolution Unit = Inch
	        [Exif Thumbnail] Thumbnail Offset = 1494 bytes
	        [Exif Thumbnail] Thumbnail Length = 6077 bytes
	    */
	    //@Test
	    //try {
	    //	nm.testExifThumbnailDirectory_MatchesKnownValues();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testExifThumbnailDirectory_MatchesKnownValues()
	    public void testExifThumbnailDirectory_MatchesKnownValues() throws Exception
	    {
	        assertEquals(6, _thumbDirectory.getInt(ExifThumbnailDirectory.TAG_COMPRESSION));
	        assertEquals(1494, _thumbDirectory.getInt(ExifThumbnailDirectory.TAG_THUMBNAIL_OFFSET));
	        assertEquals(6077, _thumbDirectory.getInt(ExifThumbnailDirectory.TAG_THUMBNAIL_LENGTH));
	        assertEquals(1494, _thumbDirectory.getInt(ExifThumbnailDirectory.TAG_THUMBNAIL_OFFSET));
	        assertEquals(72, _thumbDirectory.getInt(ExifThumbnailDirectory.TAG_X_RESOLUTION));
	        assertEquals(72, _thumbDirectory.getInt(ExifThumbnailDirectory.TAG_Y_RESOLUTION));
	        System.out.println("Finished running testExifThumbnailDirectory_MatchesKnownValues()");
	    }
	}

	/**
	 * @author psandhaus, Drew Noakes
	 */
	public class PanasonicMakernoteDescriptorTest
	{
		//MetadataExtractorTest me = new MetadataExtractorTest();
		// PanasonicMakernoteDescriptorTest pm = me.new PanasonicMakernoteDescriptorTest();
		// try {
	    //    pm.setUp();
	    // }
		// catch(Exception) {
		//    e.printStackTrace();
		//}
		//Finished running testGetDetectedFaces()
		public PanasonicMakernoteDescriptorTest() {
			
		}
	    private PanasonicMakernoteDirectory _panasonicDirectory;

	    //@Before
	    public void setUp() throws Exception
	    {
	    	ExifReaderTest er = new ExifReaderTest();
	        _panasonicDirectory = er.processBytes("C:/metadata/metadata-extractor-master/Tests/Data/withPanasonicFaces.jpg.app1", PanasonicMakernoteDirectory.class);
	    }

	    //@Test
	    //try {
	    //	pm.testGetDetectedFaces();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testGetDetectedFaces()
	    public void testGetDetectedFaces() throws Exception
	    {
	        Face expResult = new Face(142, 120, 76, 76, null, null);
	        Face[] result = _panasonicDirectory.getDetectedFaces();
	        assertNotNull(result);
	        assertEquals(expResult, result[0]);
	        System.out.println("Finished running testGetDetectedFaces()");
	    }

	    //@Test
	    //try {
	    //	pm.testGetRecognizedFaces();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testGetRecognizedFaces()
	    public void testGetRecognizedFaces() throws Exception
	    {
	        Face expResult = new Face(142, 120, 76, 76, "NIELS", new Age(31, 7, 15, 0, 0, 0));
	        Face[] result = _panasonicDirectory.getRecognizedFaces();
	        assertNotNull(result);
	        assertEquals(expResult, result[0]);
	        System.out.println("Finished running testGetRecognizedFaces()");
	    }
	}
	
	/**
	 * @author Drew Noakes https://drewnoakes.com
	 */
	public class SonyType1MakernoteTest
	{
		//MetadataExtractorTest me = new MetadataExtractorTest();
	    // SonyType1MakernoteTest sm = me.new SonyType1MakernoteTest();
	    public SonyType1MakernoteTest() {
	    	
	    }
	    //try {
	    //	sm.testSonyType1Makernote();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testSonyType1Makernote()
		public void testSonyType1Makernote() throws Exception
	    {
	    	ExifReaderTest er = new ExifReaderTest();
	        SonyType1MakernoteDirectory directory = er.processBytes("C:/metadata/metadata-extractor-master/Tests/Data/sonyType1.jpg.app1", SonyType1MakernoteDirectory.class);

	        assertNotNull(directory);
	        assertFalse(directory.hasErrors());

	        SonyType1MakernoteDescriptor descriptor = new SonyType1MakernoteDescriptor(directory);

	        assertNull(directory.getObject(SonyType1MakernoteDirectory.TAG_COLOR_TEMPERATURE));
	        assertNull(descriptor.getColorTemperatureDescription());
	        assertNull(directory.getObject(SonyType1MakernoteDirectory.TAG_SCENE_MODE));
	        assertNull(descriptor.getSceneModeDescription());
	        assertNull(directory.getObject(SonyType1MakernoteDirectory.TAG_ZONE_MATCHING));
	        assertNull(descriptor.getZoneMatchingDescription());
	        assertNull(directory.getObject(SonyType1MakernoteDirectory.TAG_DYNAMIC_RANGE_OPTIMISER));
	        assertNull(descriptor.getDynamicRangeOptimizerDescription());
	        assertNull(directory.getObject(SonyType1MakernoteDirectory.TAG_IMAGE_STABILISATION));
	        assertNull(descriptor.getImageStabilizationDescription());
	        assertNull(directory.getObject(SonyType1MakernoteDirectory.TAG_COLOR_MODE));
	        assertNull(descriptor.getColorModeDescription());

	        assertEquals("On (Shooting)", descriptor.getAntiBlurDescription());
	        assertEquals("Program", descriptor.getExposureModeDescription());
	        assertEquals("Off", descriptor.getLongExposureNoiseReductionDescription());
	        assertEquals("Off", descriptor.getMacroDescription());
	        assertEquals("Normal", descriptor.getJpegQualityDescription());
	        System.out.println("Finished running testSonyType1Makernote()");
	    }
	}

	/**
	 * @author Drew Noakes https://drewnoakes.com
	 */
	public class SonyType6MakernoteTest
	{
		//MetadataExtractorTest me = new MetadataExtractorTest();
	    // SonyType6MakernoteTest sm = me.new SonyType6MakernoteTest();
	    public SonyType6MakernoteTest() {
	    	
	    }
	    //try {
	    //	sm.testSonyType6Makernote();
	    //}
	    //catch(Exception) {
	    //	e.printStackTrace();
	    //}
	    //Finished running testSonyType6Makernote()
	    public void testSonyType6Makernote() throws Exception
	    {
	    	ExifReaderTest er = new ExifReaderTest();
	        SonyType6MakernoteDirectory directory = er.processBytes("C:/metadata/metadata-extractor-master/Tests/Data/sonyType6.jpg.app1.0", SonyType6MakernoteDirectory.class);

	        assertNotNull(directory);
	        assertFalse(directory.hasErrors());

	        SonyType6MakernoteDescriptor descriptor = new SonyType6MakernoteDescriptor(directory);

	        assertEquals("2.00", descriptor.getMakernoteThumbVersionDescription());
	        System.out.println("Finished running testSonyType6Makernote()");
	    }
	}

}