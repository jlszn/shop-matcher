package shop_matcher

// read from json to case class

// calculate distance to user

// make case classes to represent distance (Very Close, Close, etc.)
// make case classes to represent passing-by frequency

/*{
    "type":"FeatureCollection",
    "features":[
        {
            "type":"Feature",
            "geometry":{
                "type":"Point",
                "coordinates":[
                    -1.7717988686592332,
                    48.602742696725414
                ]
            },
            "properties":{
                "gml_id":"shop.1",
                "shop_id":"1",
                "shop_name":"McDonalds",
                "insee_comm":"35361",
                "por_x":297025,
                "por_y":2408370
            }
        }
    ]
}*/

// We also have  a file with people's trajectory (userid,lat,lon,timestamp,age)
// We would like to match the users with the most applicable shops based on the distance as well as on the frequency  they pass by.

case class Shop()

case class User()

object Matcher
