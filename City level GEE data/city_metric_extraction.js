var hotspots = ee.FeatureCollection("users/mitchest/ebird_hotspots/us_cities");

/*var buf_5km = ee.FeatureCollection('users/mitchest/ebird_hotspots/eb_final_hotspots5');
var buf_15km = ee.FeatureCollection('users/mitchest/ebird_hotspots/eb_final_hotspots15');
var buf_25km = ee.FeatureCollection('users/mitchest/ebird_hotspots/eb_final_hotspots25');
var hotspots = ee.FeatureCollection('users/mitchest/ebird_hotspots/eb_final_hotspots');*/

//Map.addLayer(buf_5km);
//Map.addLayer(buf_15km);
//Map.addLayer(buf_25km);
Map.addLayer(hotspots);
Map.centerObject(hotspots)

// load forest cover
var forest_loss = ee.Image('UMD/hansen/global_forest_change_2017_v1_5').select('loss');
var forest_gain = ee.Image('UMD/hansen/global_forest_change_2017_v1_5').select('gain');
//Map.addLayer({eeObject:forest_loss, name:"hansen-forest-loss"});

// load GLS (USGS/NASA)
var gls_tree = ee.ImageCollection("GLCF/GLS_TCC")
               .filterDate('2010-01-01', '2010-12-31').mosaic()
               .select('tree_canopy_cover');
var gls_water = ee.ImageCollection("GLCF/GLS_WATER")
               //.filterDate('2000-01-01', '2000-12-31')
               .mosaic()
               .select('water');
//Map.addLayer({eeObject:gls_tree, name:"gls-tree"});
//Map.addLayer({eeObject:gls_water, name:"gls-water"});

// load EVI (Landsat) - i think a better choice than NDVI in this case
// this is clunky, but might enable different date ranges and reducers in future (excuse for my rubbish code)
var evi_2013 = ee.ImageCollection('LANDSAT/LC08/C01/T1_ANNUAL_EVI')
               .filterDate('2013-01-01', '2013-12-31').mean();
var evi_2014 = ee.ImageCollection('LANDSAT/LC08/C01/T1_ANNUAL_EVI')
               .filterDate('2014-01-01', '2014-12-31').mean();
var evi_2015 = ee.ImageCollection('LANDSAT/LC08/C01/T1_ANNUAL_EVI')
               .filterDate('2015-01-01', '2015-12-31').mean();
var evi_2016 = ee.ImageCollection('LANDSAT/LC08/C01/T1_ANNUAL_EVI')
               .filterDate('2016-01-01', '2016-12-31').mean();
var evi_2017 = ee.ImageCollection('LANDSAT/LC08/C01/T1_ANNUAL_EVI')
               .filterDate('2017-01-01', '2017-12-31').mean();
var evi_2018 = ee.ImageCollection('LANDSAT/LC08/C01/T1_ANNUAL_EVI')
               .filterDate('2017-01-01', '2017-12-31').mean();

// test one evi ayer
//Map.addLayer({eeObject:evi_2013, name:"evi-2013"});

// load globcover (ESA)
var esa_globcover = ee.Image("ESA/GLOBCOVER_L4_200901_200912_V2_3")
                    .select('landcover');
//Map.addLayer({eeObject:esa_globcover, name:"globcover"});

// load US LC data - for impervious surface
var landcover2011 = ee.Image("USGS/NLCD/NLCD2011").select("impervious")

// now calculate the buffer metrics
// function to do the calculation
var reduce_by_buffer = function(image_in, buffer) {
  return image_in.reduceRegions({
    collection: buffer,
    reducer: ee.Reducer.mean(),
    scale: 30
  });
}
var reduce_by_buffer_mode = function(image_in, buffer) {
  return image_in.reduceRegions({
    collection: buffer,
    reducer: ee.Reducer.mode(),
    scale: 30
  });
}
var reduce_by_buffer_value = function(image_in, buffer, value) {
  //var image_all = image_in.gte(-1);
  var image_value = image_in.eq(value);
  //var count_all = image_in.reduceRegions({
  //  collection: buffer,
  //  reducer: ee.Reducer.sum(),
  //  scale: 30
  //});
  var count_value = image_value.reduceRegions({
    collection: buffer,
    reducer: ee.Reducer.sum(),
    scale: 30
  });
  
  var addArea = function(feature) {
    var with_total_area = feature.set({area_m2: ee.Number(feature.geometry().area())});
    return with_total_area.set({area_value_m2: ee.Number(with_total_area.get('sum')).multiply(30 * 30)});
  }
  
  return count_value.map(addArea)
  
  //var fraction = ee.Number(count_value.get('sum')).divide(ee.Number(count_all.get('sum')));
  //return ee.FeatureCollection(null, fraction)
  //return count_value.set('water_frac', fraction);
  
  //return count_all.set('water_pixels', count_value.get('sum'));
  
  //var idfilter = ee.Filter.equals({
  //leftField: 'system:index',
  //rightField: 'system:index'
  //});
  //var saveAllJoin = ee.Join.saveAll('sum_value');
  //return saveAllJoin.apply(count_all, count_value, idfilter);
}
var reduce_by_buffer_hist = function(image_in, buffer) {
  return image_in.reduceRegions({
      collection: buffer,
      reducer: ee.Reducer.frequencyHistogram(),
      scale: 30
  });
}

// function to do the export
var export_buffer_stats = function(buf_stats, out_name) {
  Export.table.toDrive({
    collection: buf_stats,
    description: out_name,
    folder: 'ebird',
    fileFormat: 'CSV'
  });
}

//print(reduce_by_buffer_value(gls_water, buf_5km, 2))
//print(reduce_by_buffer_hist(gls_water, buf_5km))

// run the calcs
// forest
/*export_buffer_stats(reduce_by_buffer(forest_loss, buf_5km), 'forest_loss_5');
export_buffer_stats(reduce_by_buffer(forest_loss, buf_15km), 'forest_loss_15');
export_buffer_stats(reduce_by_buffer(forest_loss, buf_25km), 'forest_loss_25');*/
export_buffer_stats(reduce_by_buffer(forest_loss, hotspots), 'forest_loss_hs');

/*export_buffer_stats(reduce_by_buffer(forest_gain, buf_5km), 'forest_gain_5');
export_buffer_stats(reduce_by_buffer(forest_gain, buf_15km), 'forest_gain_15');
export_buffer_stats(reduce_by_buffer(forest_gain, buf_25km), 'forest_gain_25');*/
export_buffer_stats(reduce_by_buffer(forest_gain, hotspots), 'forest_gain_hs');

// gls_tree
/*export_buffer_stats(reduce_by_buffer(gls_tree, buf_5km), 'gls_tree_5');
export_buffer_stats(reduce_by_buffer(gls_tree, buf_15km), 'gls_tree_15');
export_buffer_stats(reduce_by_buffer(gls_tree, buf_25km), 'gls_tree_25');*/
export_buffer_stats(reduce_by_buffer(gls_tree, hotspots), 'gls_tree_hs');

// gls_water
/*export_buffer_stats(reduce_by_buffer_value(gls_water, buf_5km, 2), 'gls_water_5');
export_buffer_stats(reduce_by_buffer_value(gls_water, buf_15km, 2), 'gls_water_15');
export_buffer_stats(reduce_by_buffer_value(gls_water, buf_25km, 2), 'gls_water_25');*/
export_buffer_stats(reduce_by_buffer_value(gls_water, hotspots, 2), 'gls_water_hs');

// evi
/*export_buffer_stats(reduce_by_buffer(evi_2013, buf_5km), 'evi_2013_5');
export_buffer_stats(reduce_by_buffer(evi_2013, buf_15km), 'evi_2013_15');
export_buffer_stats(reduce_by_buffer(evi_2013, buf_25km), 'evi_2013_25');*/
export_buffer_stats(reduce_by_buffer(evi_2013, hotspots), 'evi_2013_hs');

/*export_buffer_stats(reduce_by_buffer(evi_2014, buf_5km), 'evi_2014_5');
export_buffer_stats(reduce_by_buffer(evi_2014, buf_15km), 'evi_2014_15');
export_buffer_stats(reduce_by_buffer(evi_2014, buf_25km), 'evi_2014_25');*/
export_buffer_stats(reduce_by_buffer(evi_2014, hotspots), 'evi_2014_hs');

/*export_buffer_stats(reduce_by_buffer(evi_2015, buf_5km), 'evi_2015_5');
export_buffer_stats(reduce_by_buffer(evi_2015, buf_15km), 'evi_2015_15');
export_buffer_stats(reduce_by_buffer(evi_2015, buf_25km), 'evi_2015_25');*/
export_buffer_stats(reduce_by_buffer(evi_2015, hotspots), 'evi_2015_hs');

/*export_buffer_stats(reduce_by_buffer(evi_2016, buf_5km), 'evi_2016_5');
export_buffer_stats(reduce_by_buffer(evi_2016, buf_15km), 'evi_2016_15');
export_buffer_stats(reduce_by_buffer(evi_2016, buf_25km), 'evi_2016_25');*/
export_buffer_stats(reduce_by_buffer(evi_2016, hotspots), 'evi_2016_hs');

/*export_buffer_stats(reduce_by_buffer(evi_2017, buf_5km), 'evi_2017_5');
export_buffer_stats(reduce_by_buffer(evi_2017, buf_15km), 'evi_2017_15');
export_buffer_stats(reduce_by_buffer(evi_2017, buf_25km), 'evi_2017_25');*/
export_buffer_stats(reduce_by_buffer(evi_2017, hotspots), 'evi_2017_hs');

/*export_buffer_stats(reduce_by_buffer(evi_2018, buf_5km), 'evi_2018_5');
export_buffer_stats(reduce_by_buffer(evi_2018, buf_15km), 'evi_2018_15');
export_buffer_stats(reduce_by_buffer(evi_2018, buf_25km), 'evi_2018_25');*/
export_buffer_stats(reduce_by_buffer(evi_2018, hotspots), 'evi_2018_hs');

// esa glob cover
/*export_buffer_stats(reduce_by_buffer_mode(esa_globcover, buf_5km), 'esa_globcover_5');
export_buffer_stats(reduce_by_buffer_mode(esa_globcover, buf_15km), 'esa_globcover_15');
export_buffer_stats(reduce_by_buffer_mode(esa_globcover, buf_25km), 'esa_globcover_25');*/
export_buffer_stats(reduce_by_buffer_mode(esa_globcover, hotspots), 'esa_globcover_hs');

// US landcover - impervious
export_buffer_stats(reduce_by_buffer(landcover2011, hotspots), 'nlcd_impervious_hs');