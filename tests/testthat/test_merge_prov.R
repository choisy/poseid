library(dplyr) #for 'filter'
library(dictionary) # for 'province_year'
library(magrittr) # for '%>%'
library(gso) # for the data table
library(gdpm) # for 'getid'
library(tidyr) # for 'replace_na'

context("Test if `merge_prov` returns the correct output")

# test merge_prov sum  ---------------------------------------------------------

test_that("`merge_prov` sums correcty", {

  test <- gso::content %>% filter(data_name == "education_3") %>%
    .$data %>% .[[1]]

  # FROM >= 1992
  df <- gso::content %>% filter(data_name == "education_3") %>%
    .$data %>% .[[1]] %>%
    merge_prov(FUN = sum, from = "1992-01-01", to = "2015-12-31")

  # Ha Noi
  expect_equal(df %>%
    filter(province == "Ha Noi", year == 2007, key == "total") %>% .$value,
    test %>% filter(province == "Ha Noi", year == 2007) %>% .$total +
    test %>% filter(province == "Ha Tay", year == 2007) %>% .$total)

  # Can Tho
  expect_equal(df %>%
    filter(province == "Can Tho", year == 2005, key == "total") %>% .$value,
    test %>% filter(province == "Can Tho", year == 2005) %>% .$total +
    test %>% filter(province == "Hau Giang", year == 2005) %>% .$total)

  # Dack Lak
  expect_equal(df %>%
    filter(province == "Dack Lak", year == 2006, key == "total") %>% .$value,
    test %>% filter(province == "Dak Lak", year == 2006) %>% .$total +
    test %>% filter(province == "Dak Nong", year == 2006) %>% .$total)

  # Yen Bai
  expect_equal(df %>%
    filter(province == "Yen Bai", year == 2004, key == "total") %>% .$value,
    test %>% filter(province == "Yen Bai", year == 2004) %>% .$total)


  # FROM < 1992
  df <- gso::content %>% filter(data_name == "education_3") %>%
    .$data %>% .[[1]] %>%
     merge_prov(FUN = sum, from = "1990-01-01", to = "2015-12-31")

  # Hau Giang
  expect_equal(df %>%
    filter(province == "Hau Giang", year == 2002, key == "total") %>% .$value,
    test %>% filter(province == "Can Tho", year == 2002) %>% .$total +
    test %>% filter(province == "Soc Trang", year == 2002) %>% .$total)

# Hau Giang
  expect_equal(df %>%
    filter(province == "Hau Giang", year == 2005, key == "total") %>% .$value,
    test %>% filter(province == "Can Tho", year == 2005) %>% .$total +
    test %>% filter(province == "Soc Trang", year == 2005) %>% .$total +
    test %>% filter(province == "Hau Giang", year == 2005) %>% .$total)

# Ha Noi
  expect_equal(df %>%
    filter(province == "Ha Noi", year == 2010, key == "total") %>% .$value,
    test %>% filter(province == "Ha Noi", year == 2010) %>% .$total +
    test %>% filter(province == "Hoa Binh", year == 2010) %>% .$total)

# Ha Noi
  expect_equal(df %>%
    filter(province == "Ha Noi", year == 2007, key == "total") %>% .$value,
    test %>% filter(province == "Ha Noi", year == 2007) %>% .$total +
    test %>% filter(province == "Ha Tay", year == 2007) %>% .$total +
    test %>% filter(province == "Hoa Binh", year == 2007) %>% .$total)


# From <= 1989
  df <- gso::content %>% filter(data_name == "education_3") %>%
    .$data %>% .[[1]] %>%
    merge_prov(FUN = sum, from = "1989-01-01", to = "2015-12-31")

# Binh Tri Thien
  expect_equal(df %>%
    filter(province == "Binh Tri Thien", year == 2007, key == "total") %>%
      .$value,
    test %>% filter(province == "Quang Binh", year == 2007) %>% .$total +
    test %>% filter(province == "Quang Tri", year == 2007) %>% .$total +
    test %>% filter(province == "Thua Thien Hue", year == 2007) %>% .$total)

})

# test NA-----------------------------------------------------------------------
test_that("`merge_prov` sums correcty and returns NA when one of the province
          merge back together as an NA value", {

  test <- gso::content %>% filter(data_name == "agriculture_22") %>%
    .$data %>% .[[1]]
  df <- gso::content %>% filter(data_name == "agriculture_22") %>%
    .$data %>% .[[1]] %>%
    merge_prov(FUN = sum, from = "1980-01-01", to = "2015-12-31")

  expect_equal(df %>%
    filter(province == "Hau Giang", year > 1994) %>%
    select(value) %>% unlist %>% as.vector,
    c(138.8, 118.4, 129.3, 82.1, rep(NA, 17)))

  df <- gso::content %>% filter(data_name == "agriculture_22") %>%
    .$data %>% .[[1]] %>%
    merge_prov(FUN = sum, from = "1980-01-01", to = "2016-12-31", na.rm = TRUE)

  expect_equal(df %>% filter(province == "Hau Giang", year > 1994) %>%
                 select(value) %>% unlist %>% as.vector,
               test %>% filter(province == "Can Tho", year  > 1994) %>%
                 .$planted_area_of_winter_paddy %>% replace_na(0) +
               test %>% filter(province == "Soc Trang", year > 1994) %>%
                 .$planted_area_of_winter_paddy %>% replace_na(0))

})

# test provinces names ---------------------------------------------------------
test_that("`merge_prov` merges back together the good provinces", {

  # < 1990
  df <- gso::content %>% filter(data_name == "education_3") %>%
    .$data %>% .[[1]] %>%
    merge_prov(FUN = sum, from = "1980-01-01", to = "2015-12-31")

  expect_identical(c(unique(df$province), "Ha Son Binh") %>% sort,
                   dictionary::vn_admin1_year$`1979-1990` %>% unique)

  # 1990
  df <- gso::content %>% filter(data_name == "education_3") %>%
    .$data %>% .[[1]] %>%
    merge_prov(FUN = sum, from = "1990-01-01", to = "2015-12-31")

  expect_identical(c(unique(df$province), "Ha Son Binh") %>% sort,
                   dictionary::vn_admin1_year$`1990-1991` %>% unique)

  df <- gso::content %>% filter(data_name == "education_3") %>%
    .$data %>% .[[1]] %>%
    merge_prov(FUN = sum, from = "1991-01-01", to = "2015-12-31")

  expect_identical(c(unique(df$province), "Ha Son Binh") %>% sort,
                   dictionary::vn_admin1_year$`1991-1992` %>% unique)

  df <- gso::content %>% filter(data_name == "education_3") %>%
    .$data %>% .[[1]] %>%
    merge_prov(FUN = sum, from = "1992-01-01", to = "2015-12-31")

  expect_identical(c(unique(df$province), "Ha Tay") %>% sort,
                   dictionary::vn_admin1_year$`1992-1997` %>% unique)

  df <- gso::content %>% filter(data_name == "education_3") %>%
    .$data %>% .[[1]] %>%
    merge_prov(FUN = sum, from = "1997-01-01", to = "2015-12-31")

  expect_identical(c(unique(df$province), "Ha Tay") %>% sort,
                   dictionary::vn_admin1_year$`1997-2004` %>% unique)

  df <- gso::content %>% filter(data_name == "education_3") %>%
    .$data %>% .[[1]] %>%
    merge_prov(FUN = sum, from = "2004-01-01", to = "2015-12-31")

  expect_identical(c(unique(df$province), "Ha Tay") %>% sort,
                   dictionary::vn_admin1_year$`2004-2008` %>% unique)

  df <- gso::content %>% filter(data_name == "education_3") %>%
    .$data %>% .[[1]] %>%
    merge_prov(FUN = sum, from = "2008-01-01", to = "2015-12-31")

  expect_identical(c(unique(df$province)) %>% sort,
                   dictionary::vn_admin1_year$`2008-2020` %>% unique)

})

# test merge_prov  -------------------------------------------------------------
test_that("`merge_prov` applies weighted mean correcty", {

  test <- gso::content %>% filter(data_name == "agriculture_22") %>%
    .$data %>% .[[1]]
  pop_size <- gso::content %>% filter(data_name == "demography_5") %>%
    .$data %>% .[[1]]%>% dplyr::select(province, year, total) %>%
    mutate(year = as.numeric(year))


  # FROM >= 1992
  df <- gso::content %>% filter(data_name == "agriculture_22") %>%
    .$data %>% .[[1]] %>%
    merge_prov(FUN = weighted.mean,
               from = "1992-01-01", to = "2015-12-31",
               df2 = pop_size, args = "total")


  # Ha Noi
  expect_equal(df %>%
          filter(province == "Ha Noi", year == 2007) %>% .$value,
          weighted.mean(c(
            test %>% filter(province == "Ha Noi", year == 2007) %>% .[,3],
            test %>% filter(province == "Ha Tay", year == 2007) %>% .[,3]),
            c(pop_size %>% filter(province == "Ha Noi", year == 2007) %>%
                .["total"],
              pop_size %>% filter(province == "Ha Tay", year == 2007) %>%
                .["total"]) %>% unlist()))

  # Can Tho NA
  expect_equal(df %>%
          filter(province == "Can Tho", year == 2005) %>% .$value,
          weighted.mean(c(
            test %>% filter(province == "Can Tho", year == 2005) %>% .[,3],
            test %>% filter(province == "Hau Giang", year == 2005) %>% .[,3]),
            c(pop_size %>% filter(province == "Can Tho", year == 2005) %>%
                .["total"],
              pop_size %>% filter(province == "Hau Giang", year == 2005) %>%
                .["total"]) %>% unlist()
            ))

  # Dack Lak
  expect_equal(df %>%
          filter(province == "Dack Lak", year == 2006) %>% .$value,
                         weighted.mean(c(
          test %>% filter(province == "Dak Lak", year == 2006) %>% .[,3],
          test %>% filter(province == "Dak Nong", year == 2006) %>% .[,3]),
          c(pop_size %>% filter(province == "Dak Lak", year == 2006) %>%
              .["total"],
            pop_size %>% filter(province == "Dak Nong", year == 2006) %>%
              .["total"]) %>% unlist()
                         ))

  # Yen Bai
  expect_equal(df %>%
          filter(province == "Yen Bai", year == 2004) %>% .$value,
                         weighted.mean(
          test %>% filter(province == "Yen Bai", year == 2004) %>% .[,3],
          pop_size %>% filter(province == "Yen Bai", year == 2004) %>%
            .["total"]))


  # FROM < 1992

  df <- gso::content %>% filter(data_name == "agriculture_22") %>%
    .$data %>% .[[1]] %>%
    merge_prov(FUN = weighted.mean,
               from = "1990-01-01", to = "2015-12-31",
               df2 = pop_size, args = "total")

  # Hau Giang NA
  expect_equal(df %>%
          filter(province == "Hau Giang", year == 2002) %>% .$value,
                         weighted.mean(c(
          test %>% filter(province == "Can Tho", year == 2002) %>% .[,3],
          test %>% filter(province == "Soc Trang", year == 2002) %>% .[,3]),
          c(pop_size %>% filter(province == "Can Tho", year == 2002) %>%
              .["total"],
            pop_size %>% filter(province == "Soc Trang", year == 2002) %>%
              .["total"]) %>% unlist()
                         ))

  # Hau Giang NA
  expect_equal(df %>%
          filter(province == "Hau Giang", year == 2005) %>% .$value,
                         weighted.mean(c(
          test %>% filter(province == "Can Tho", year == 2005) %>% .[,3],
          test %>% filter(province == "Hau Giang", year == 2005) %>% .[,3],
          test %>% filter(province == "Soc Trang", year == 2005) %>% .[,3]),
          c(pop_size %>% filter(province == "Can Tho", year == 2005) %>%
              .["total"],
            pop_size %>% filter(province == "Hau Giang", year == 2005) %>%
              .["total"],
            pop_size %>% filter(province == "Soc Trang", year == 2005) %>%
              .["total"]) %>% unlist()
                         ))

  # Ha Noi
  expect_equal(df %>%
          filter(province == "Ha Noi", year == 2010) %>% .$value,
                         weighted.mean(c(
          test %>% filter(province == "Ha Noi", year == 2010) %>% .[,3],
          test %>% filter(province == "Hoa Binh", year == 2010) %>% .[,3]),
          c(pop_size %>% filter(province == "Ha Noi", year == 2010) %>%
              .["total"],
            pop_size %>% filter(province == "Hoa Binh", year == 2010) %>%
              .["total"]) %>% unlist()
                         ))

  # Ha Noi
  expect_equal(df %>%
          filter(province == "Ha Noi", year == 2007) %>% .$value,
                         weighted.mean(c(
          test %>% filter(province == "Ha Noi", year == 2007) %>% .[,3],
          test %>% filter(province == "Ha Tay", year == 2007) %>% .[,3],
          test %>% filter(province == "Hoa Binh", year == 2007) %>% .[,3]),
          c(pop_size %>% filter(province == "Ha Noi", year == 2007) %>%
              .["total"],
            pop_size %>% filter(province == "Ha Tay", year == 2007) %>%
              .["total"],
            pop_size %>% filter(province == "Hoa Binh", year == 2007) %>%
              .["total"]) %>% unlist())
  )

  # From <= 1990

  df <- gso::content %>% filter(data_name == "agriculture_22") %>%
    .$data %>% .[[1]] %>%
    merge_prov(FUN = weighted.mean,
               from = "1989-01-01", to = "2015-12-31",
               df2 = pop_size, args = "total")

  # Binh Tri Thien
  expect_equal(df %>%
          filter(province == "Binh Tri Thien", year == 2007) %>% .$value,
                         weighted.mean(c(
          test %>% filter(province == "Quang Binh", year == 2007) %>% .[,3],
          test %>% filter(province == "Quang Tri", year == 2007) %>% .[,3],
          test %>% filter(province == "Thua Thien Hue", year == 2007) %>%
            .[,3]),
          c(pop_size %>% filter(province == "Quang Binh", year == 2007) %>%
              .["total"],
            pop_size %>% filter(province == "Quang Tri", year == 2007) %>%
              .["total"],
            pop_size %>% filter(province == "Thua Thien Hue",
           year == 2007) %>% .["total"]) %>% unlist()
                         ))
})


# test splits list in merge_prov -----------------------------------------------
test_that("`merge_prov` follow the best list", {

  test <- gso::content %>% filter(data_name == "education_3") %>%
    .$data %>% .[[1]]

  # FROM >= 1992
  df <- gso::content %>% filter(data_name == "education_3") %>%
    .$data %>% .[[1]] %>%
   merge_prov(FUN = sum, diseases = c("hepatitis", "cholera", "ili"),
                          from = "1992-01-01", to = "2015-12-31")

  # Ha Noi
  expect_equal(df %>%
      filter(province == "Ha Noi", year == 2007, key == "total") %>% .$value,
      test %>% filter(province == "Ha Noi", year == 2007) %>% .$total +
      test %>% filter(province == "Ha Tay", year == 2007) %>% .$total)

  # Can Tho
  expect_equal(df %>%
      filter(province == "Can Tho", year == 2005, key == "total") %>% .$value,
      test %>% filter(province == "Can Tho", year == 2005) %>% .$total +
      test %>% filter(province == "Hau Giang", year == 2005) %>% .$total)

  # Dack Lak
  expect_equal(df %>%
      filter(province == "Dack Lak", year == 2006, key == "total") %>% .$value,
      test %>% filter(province == "Dak Lak", year == 2006) %>% .$total +
      test %>% filter(province == "Dak Nong", year == 2006) %>% .$total)

  # Yen Bai
  expect_equal(df %>%
      filter(province == "Yen Bai", year == 2004, key == "total") %>% .$value,
      test %>% filter(province == "Yen Bai", year == 2004) %>% .$total)


  # FROM < 1992
  df <- gso::content %>% filter(data_name == "education_3") %>%
    .$data %>% .[[1]] %>%
   merge_prov(FUN = sum, diseases = c("hepatitis", "cholera", "ili"),
                          from = "1990-01-01", to = "2015-12-31")

  # Dack Lak
  expect_equal(df %>%
      filter(province == "Dack Lak", year == 2006, key == "total") %>% .$value,
      test %>% filter(province == "Dak Lak", year == 2006) %>% .$total +
      test %>% filter(province == "Dak Nong", year == 2006) %>% .$total)

  # Hau Giang
  expect_equal(df %>%
      filter(province == "Hau Giang", year == 2002, key == "total") %>% .$value,
      test %>% filter(province == "Can Tho", year == 2002) %>% .$total +
      test %>% filter(province == "Soc Trang", year == 2002) %>% .$total)

  # Hau Giang
  expect_equal(df %>%
      filter(province == "Hau Giang", year == 2005, key == "total") %>% .$value,
      test %>% filter(province == "Can Tho", year == 2005) %>% .$total +
      test %>% filter(province == "Soc Trang", year == 2005) %>% .$total +
      test %>% filter(province == "Hau Giang", year == 2005) %>% .$total)

  # Ha Noi
  expect_equal(df %>%
      filter(province == "Ha Noi", year == 2010, key == "total") %>% .$value,
      test %>% filter(province == "Ha Noi", year == 2010) %>% .$total +
      test %>% filter(province == "Hoa Binh", year == 2010) %>% .$total)

  # Ha Noi
  expect_equal(df %>%
      filter(province == "Ha Noi", year == 2007, key == "total") %>% .$value,
      test %>% filter(province == "Ha Noi", year == 2007) %>% .$total +
      test %>% filter(province == "Ha Tay", year == 2007) %>% .$total +
      test %>% filter(province == "Hoa Binh", year == 2007) %>% .$total)


  # From <= 1990
  df <- gso::content %>% filter(data_name == "education_3") %>%
    .$data %>% .[[1]] %>%
   merge_prov(FUN = sum, diseases = c("hepatitis", "cholera", "ili"),
                          from = "1990-01-01", to = "2015-12-31")

  # Binh Tri Thien
  expect_equal(df %>%
      filter(province == "Binh Tri Thien", year == 2007, key == "total") %>%
      .$value,
      test %>% filter(province == "Quang Binh", year == 2007) %>% .$total +
      test %>% filter(province == "Quang Tri", year == 2007) %>% .$total +
      test %>% filter(province == "Thua Thien Hue", year == 2007) %>% .$total)

  # Province
  df <- gso::content %>% filter(data_name == "education_3") %>%
    .$data %>% .[[1]] %>%
   merge_prov(FUN = sum, diseases = c("hepatitis", "cholera", "ili"),
                          from = "1990-01-01", to = "2015-12-31")
  expect_equal(
    mean(df$province %in%
           dictionary::vn_admin1_year$`1979-1990`), 1)


  df <- gso::content %>% filter(data_name == "education_3") %>%
    .$data %>% .[[1]] %>%
   merge_prov(FUN = sum, diseases = c("hepatitis", "cholera", "ili"),
                          from = "1991-01-01", to = "2015-12-31")
  expect_equal(
    mean(df$province %in%
           dictionary::vn_admin1_year$`1991-1992`), 1)

})

# test merge_prov with monthly data---------------------------------------------
test_that("`merge_prov` returns the correct values for monthly data", {

  dyear <-  seq(1989, 2009)
  chickenpox <-  NULL
  for (i in seq_along(dyear)[-length(dyear)]){
    c <-  gdpm::getid(chickenpox, from = dyear[i],
                      to = paste0(dyear[i], "-12-01"))
    chickenpox <-  rbind(chickenpox, c) %>%
      dplyr::filter(duplicated(.) == FALSE)
  }

  chickenpox0 <- tidyr::replace_na(chickenpox,
                                   list(incidence_chickenpox = 0))

  expect_equal(
    merge_prov(chickenpox, from = "2000-01-01", to = "2000-12-01") %>%
      dplyr::filter(province == "Cao Bang", key == "incidence_chickenpox") %>%
      .$value,
    c(0, 21, 0, 0, 0 ,7 , 0, 0, 1, 0, 0, 0))

  expect_equal(
    merge_prov(chickenpox, from = "1991-01-01", to = "1992-12-01") %>%
      dplyr::filter(province == "Ha Nam Ninh",
                    key == "incidence_chickenpox") %>%
      .$value,
    c(75, 0, 36, 0, 57, 69, 122, 88, 98, 119, 65, 27, 21, 18, 62, 59, 74, 89,
      144, 67, 59, 37, 22, 24))

  expect_equal(
    merge_prov(chickenpox, from = "1996-01-01", to = "1997-12-01") %>%
      dplyr::filter(province == "Nam Ha",
                    key == "incidence_chickenpox") %>%
      .$value,
    c(17, 24, 57, 8, 0, 6, 7, 19, 17, 12, 39, 5, 67, 18, 35, 21, 12, 13, 31, 29,
      20, 6, 22, 10))

  expect_equal(
    merge_prov(chickenpox, from = "1991-01-01", to = "1997-12-01") %>%
      dplyr::filter(province == "Ha Nam Ninh",
                    key == "incidence_chickenpox") %>%
      .$value,
    c(75, 0, 36, 0, 57, 69, 122, 88, 98, 119, 65, 27, 21, 18, 62, 59, 74, 89,
      144, 67, 59, 37, 22, 24, 17, 11, 52, 9, 34, 78, 142, 141, 131, 39, 13, 23,
      82, 15, 69, 65, 54, 60, 83, 56, 21, 15, 15, 1, 7, 17, 45, 51, 31, 36, 69,
      38, 35, 27, 16, 21, 22, 38, 57, 12, 6, 6, 26, 27, 17, 12, 39, 6, 70, 18,
      39, 21, 18, 20, 33, 35, 25, 6, 23, 10))

  expect_equal(
    merge_prov(chickenpox0, from = "1989-01-01", to = "1990-12-01") %>%
                  dplyr::filter(province == "Binh Tri Thien",
                                key == "incidence_chickenpox") %>%
                  .$value,
    c(8, 6, 4, 25, 0, 0, 0, 17, 30, 1, 3, 0, 10, 6, 3, 8, 4, 2, 0, 0, 0,
      0, 0, 13))

  expect_equal(
    merge_prov(chickenpox, from = "2007-01-01", to = "2008-12-01") %>%
      dplyr::filter(province == "Ha Noi",
                    key == "incidence_chickenpox") %>%
      .$value,
    c(273, 1315, 1569, 674, 461, 347, 244, 227, 51, 52, 143, 192, 680, 653,
      1260, 988, 459, 406, 289, 84, 714, 106, 89, 85))

  expect_equal(
    merge_prov(chickenpox0, from = "1991-01-01", to = "2008-12-01") %>%
      dplyr::filter(province == "Ha Noi",
                    key == "incidence_chickenpox") %>%
      .$value,
    c(164, 52, 107, 132, 162, 106, 58, 111, 56, 120, 30, 22, 57, 119, 134, 117,
      70, 53, 32, 69, 66, 23, 21, 44, 42, 34, 39, 102, 134, 107, 38, 97, 91, 46,
      33, 27, 67, 96, 130, 72, 46, 64, 44, 16, 19, 18, 51, 35, 20, 39, 38, 53,
      21, 46, 57, 49, 23, 29, 18, 40, 62, 168, 92, 87, 102, 59, 92, 103, 490,
      284, 58, 31, 36, 40, 61, 69, 22, 109, 14, 12, 28,  9,  5, 29, 58, 32, 30,
      33,  7, 68, 39, 28, 18, 24, 19, 17, 68, 98, 79, 93, 65, 39, 31, 28, 15,
      20, 18, 14, 14, 43,  3, 113, 16, 39, 24, 35, 19, 15, 24, 37, 102, 99, 205,
      91, 57, 54, 57, 40, 33, 14, 21, 39, 30, 43, 193, 23, 62, 59, 17, 17, 29,
      43, 46, 28, 93, 62, 11, 13, 74, 114, 61, 70, 34, 71, 31, 108, 204, 579,
      995, 844, 424, 411, 235, 158, 134, 79, 184, 140, 186, 79, 235, 406, 343,
      268, 167, 168, 261, 82, 97, 235, 146, 511, 644, 657, 304, 236, 127, 115,
      100, 81, 56, 109, 287, 1317, 1615, 758, 472, 393, 272, 242, 63, 63, 155,
      267, 773, 668, 1304, 1152, 545, 505, 307, 102, 725, 111, 99, 105))

})

