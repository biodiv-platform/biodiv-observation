package com.strandls.observation.util;

import com.strandls.observation.pojo.DataTable;
import com.strandls.observation.pojo.ObservationBulkData;
import com.strandls.resource.pojo.License;
import com.strandls.taxonomy.pojo.SpeciesGroup;
import com.strandls.traits.pojo.TraitsValuePair;
import com.strandls.userGroup.pojo.UserGroupIbp;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;

import javax.servlet.http.HttpServletRequest;
import java.io.File;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.BlockingQueue;

public class FileTask implements Runnable {
    private final BlockingQueue<ObservationBulkData> queue;
    private final String filePath;
    private final Map<String, Integer> fieldMapping;
    private final HttpServletRequest request;
    private final DataTable dataTable;
    private final List<SpeciesGroup> speciesGroupList;
    private final List<TraitsValuePair> pairs;
    private final List<UserGroupIbp> userGroupsList;
    private final List<License> licenses;

    public FileTask(BlockingQueue<ObservationBulkData> queue, Map<String, Integer> fieldMapping, String filePath,
                    HttpServletRequest request, DataTable dataTable, List<SpeciesGroup> speciesGroupList,
                    List<TraitsValuePair> pairs, List<UserGroupIbp> userGroupsList, List<License> licenses) {
        this.queue = queue;
        this.fieldMapping = fieldMapping;
        this.filePath = filePath;
        this.request = request;
        this.dataTable = dataTable;
        this.speciesGroupList = speciesGroupList;
        this.pairs = pairs;
        this.userGroupsList = userGroupsList;
        this.licenses = licenses;
    }

    @Override
    public void run() {
        try (XSSFWorkbook workbook = new XSSFWorkbook(new File(filePath))) {
            XSSFSheet sheet = workbook.getSheetAt(0);
            Iterator<Row> rows = sheet.rowIterator();

            Row dataRow;
            // skip header
            rows.next();

            while (rows.hasNext()) {
                dataRow = rows.next();
                ObservationBulkData observationBulkData = new ObservationBulkData(fieldMapping, dataRow,
                        request, dataTable, speciesGroupList, pairs, userGroupsList, licenses,false, fieldMapping);
                queue.put(observationBulkData);
            }			
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
}
