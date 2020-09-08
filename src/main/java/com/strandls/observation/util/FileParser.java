package com.strandls.observation.util;

public class FileParser {
	
//	public static List<Map<String, Object>> parseFile(String filePath, BulkObservationDTO observationDTO) throws Exception {
//		List<Map<String, Object>> data = null;
//		String extension = Files.getFileExtension(filePath).toLowerCase();
//		switch (extension) {
//		case "xlsx":
//			data = parseXLSXFile(filePath, observationDTO);
//			break;
//		case "xls":
//			data = parseXLSFile(filePath, observationDTO);
//			break;
//		default:
//			break;
//		}
//		return data;
//	}
//	
//	public static List<Map<String, Object>> parseXLSXFile(String filePath, BulkObservationDTO observationDTO) throws InvalidFormatException, IOException {
//		List<Map<String, Object>> data = new ArrayList<>();
//		Map<String, Integer> mappedColumns = observationDTO.getColumns(); 
//		File file = new File(filePath);
//		if (!file.exists()) {
//			throw new FileNotFoundException();
//		}
//		XSSFWorkbook workbook = new XSSFWorkbook(file);
//		XSSFSheet sheet = workbook.getSheetAt(0);
//		Iterator<Row> rows = sheet.iterator();
//		// Skip Heading
//		rows.next();
//		while (rows.hasNext()) {
//			Row row = rows.next();
//			Iterator<Cell> cells = row.cellIterator();
//			Map<String, Object> mRow = new HashMap<String, Object>();
//			int cellIndex = 0;
//			while (cells.hasNext()) {
//				if (mappedColumns.containsKey(cellIndex++)) continue; // skip column if it is not mapped
//				Cell cell = cells.next();
//				switch (cell.getCellType()) {
//				case STRING:
//					mRow.put(mappedColumns.get(cellIndex), cell.getStringCellValue());
//					break;
//				case BOOLEAN:
//					mRow.put(mappedColumns.get(cellIndex), cell.getBooleanCellValue());
//					break;
//				case NUMERIC:
//					if (DateUtil.isCellDateFormatted(cell)) {
//						mRow.put(mappedColumns.get(cellIndex), cell.getDateCellValue());						
//					} else {
//						mRow.put(mappedColumns.get(cellIndex), cell.getNumericCellValue());						
//					}
//					break;
//				case BLANK:
//					continue;
//				default:
//					continue;
//				}
//			}
//			data.add(mRow);
//		}
//		workbook.close();
//		return data;		
//	}
//	
//	public static List<Map<String, Object>> parseXLSFile(String filePath, BulkObservationDTO observationDTO) throws InvalidFormatException, IOException {
//		List<Map<String, Object>> data = new ArrayList<>();
//		Map<Integer, String> mappedColumns = observationDTO.getColumns(); 
//		File file = new File(filePath);
//		if (!file.exists()) {
//			throw new FileNotFoundException();
//		}
//		InputStream fis = new FileInputStream(file);
//		HSSFWorkbook workbook = new HSSFWorkbook(fis);
//		HSSFSheet sheet = workbook.getSheetAt(0);
//		Iterator<Row> rows = sheet.iterator();
//		// Skip Heading
//		rows.next();
//		while (rows.hasNext()) {
//			Row row = rows.next();
//			Iterator<Cell> cells = row.cellIterator();
//			Map<String, Object> mRow = new HashMap<String, Object>();
//			int cellIndex = 0;
//			while (cells.hasNext()) {
//				if (mappedColumns.containsKey(cellIndex++)) continue; // skip column if it is not mapped
//				Cell cell = cells.next();
//				switch (cell.getCellType()) {
//				case STRING:
//					mRow.put(mappedColumns.get(cellIndex), cell.getStringCellValue());
//					break;
//				case BOOLEAN:
//					mRow.put(mappedColumns.get(cellIndex), cell.getBooleanCellValue());
//					break;
//				case NUMERIC:
//					if (DateUtil.isCellDateFormatted(cell)) {
//						mRow.put(mappedColumns.get(cellIndex), cell.getDateCellValue());						
//					} else {
//						mRow.put(mappedColumns.get(cellIndex), cell.getNumericCellValue());						
//					}
//					break;
//				case BLANK:
//					continue;
//				default:
//					continue;
//				}
//			}
//			data.add(mRow);
//		}
//		fis.close();
//		workbook.close();
//		return data;		
//	}

}
