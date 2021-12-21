kakao_api = "API_KEY"

library(dplyr)
library(jsonlite)
library(httr)

#Kakao API로부터 주소 -> 좌표 추출하는 함수
get_coord_from_addr <- function(addr, kakao_api) {

  data_list <-
    GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
        query = list(query = addr),
        add_headers(Authorization = paste0("KakaoAK ", kakao_api))) %>% 
    content(as = 'text') %>% 
    fromJSON()
  
  
  lon_lat_df <- tibble(주소 = addr, 
                       위도 = as.numeric(data_list$documents$y),
                       경도 = as.numeric(data_list$documents$x))
  
  return(lon_lat_df)
}

#Kakao API로부터 주소 -> 행정동 추출하는 함수
get_hdong_from_addr <- function(addr, kakao_api) {
  
  data_list <-
    GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
        query = list(query = addr),
        add_headers(Authorization = paste0("KakaoAK ", kakao_api))) %>% 
    content(as = 'text') %>% 
    fromJSON()
  
  if (data_list$documents %>% length() == 0){
    dong_df <- tibble(ADDRESS = addr, 
                      행정동코드 = NA,
                      행정동명 = NA)
  }else if(data_list$documents$address %>% is.na){
    dong_df <- tibble(ADDRESS = addr, 
                      행정동코드 = NA,
                      행정동명 = NA)
  }else{
    
    dong_df <- tibble(ADDRESS = addr, 
                      행정동코드 = as.numeric(data_list$documents$address$h_code) %/% 100,
                      행정동명 = as.character(data_list$documents$address$region_3depth_h_name))
  }
  return(dong_df)
}

#좌표로 행정동 받아오는 함수
get_hdong_from_coord <- function(long, lat, kakao_api) {
  
  data_list <-
    GET(url = 'https://dapi.kakao.com/v2/local/geo/coord2regioncode.json?input_coord=WGS84&output_coord=WGS84',
        query = list(x=long,
                     y=lat),
        add_headers(Authorization = paste0("KakaoAK ", kakao_api))) %>% 
    content(as = 'text') %>% 
    fromJSON()
  
  dong_df <- tibble(long = long,
                    lat = lat,
                    행정동코드 = as.numeric(data_list$documents$code[2]) %/% 100,
                    행정동명 = as.character(data_list$documents$region_3depth_name[2]))
  return(dong_df)
}

#좌표로 법정동 받아오자,,,,,,
get_bdong_from_coord <- function(long, lat, kakao_api) {
  
  data_list <-
    GET(url = 'https://dapi.kakao.com/v2/local/geo/coord2regioncode.json?input_coord=WGS84&output_coord=WGS84',
        query = list(x=long,
                     y=lat),
        add_headers(Authorization = paste0("KakaoAK ", kakao_api))) %>% 
    content(as = 'text') %>% 
    fromJSON()
  
  dong_df <- tibble(long = x,
                    lat = y,
                    행정동코드 = as.numeric(data_list$documents$code[1]) %/% 100,
                    행정동명 = as.character(data_list$documents$region_3depth_name[1]))
  return(dong_df)
}

#좌표로 행정동 주소 전체 받아오자
get_addr_from_coord <- function(long, lat, kakao_api) {
  
  data_list <-
    GET(url = 'https://dapi.kakao.com/v2/local/geo/coord2regioncode.json?input_coord=WGS84&output_coord=WGS84',
        query = list(x=long,
                     y=lat),
        add_headers(Authorization = paste0("KakaoAK ", kakao_api))) %>% 
    content(as = 'text') %>% 
    fromJSON()
  
  dong_df <- tibble(long = long,
                    lat = lat,
                    주소 = data_list$documents$address_name[2])
  return(dong_df)
}

