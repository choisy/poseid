library(dplyr) #for 'filter'
library(dictionary) # for 'province_year'
library(magrittr) # for '%>%'
library(gso) # for 'get_gso'

# Prerequisite -----------------------------------------------------------------
p_list <- gso::data_frame_summary %>%
  dplyr::filter(`priority` == "1") %>%
  dplyr::filter(`spatial resolution` == "province") %>%
  dplyr::select(`data frame`) %>%
  unlist %>%
  as.vector()

context("`merge_prov` returns the correct output")

test_that("`merge_prov` sums correcty", {

  test <- get_gso(p_list[88])

  # FROM >= 1992
  df <- get_gso(p_list[88]) %>%
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
  df <- get_gso(p_list[88]) %>%
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
  df <- get_gso(p_list[88]) %>%
    merge_prov(FUN = sum, from = "1989-01-01", to = "2015-12-31")

# Binh Tri Thien
  expect_equal(df %>%
    filter(province == "Binh Tri Thien", year == 2007, key == "total") %>%
      .$value,
    test %>% filter(province == "Quang Binh", year == 2007) %>% .$total +
    test %>% filter(province == "Quang Tri", year == 2007) %>% .$total +
    test %>% filter(province == "Thua Thien - Hue", year == 2007) %>% .$total)

})

# test NA-----------------------------------------------------------------------
test_that("`merge_prov` sums correcty and returns NA when one of the province
          merge back together as an NA value", {

  test <- get_gso(p_list[33])
  df <- get_gso(p_list[33]) %>%
    merge_prov(FUN = sum, from = "1980-01-01", to = "2015-12-31")

  expect_identical(df %>%
    filter(province == "Hau Giang", year > 1994) %>%
    select(value) %>% unlist %>% as.vector,
    c(66.6, 62.6, 58.1, 55.3,rep(NA, 17)))

})

# test provinces names ---------------------------------------------------------
test_that("`merge_prov` merges back together the good provinces", {

  # < 1990
  df <- get_gso(p_list[88]) %>%
    merge_prov(FUN = sum,
                          from = "1980-01-01", to = "2015-12-31")
  expect_equal(
    mean(df$province %in%
           dictionary::province_year$`1979` %>% unique), 1)

  # 1990
  df <- get_gso(p_list[88]) %>%
    merge_prov(FUN = sum,
                          from = "1990-01-01", to = "2015-12-31")
  expect_equal(
    mean(df$province %in%
           dictionary::province_year$`1990` %>% unique), 1)

  df <- get_gso(p_list[88]) %>%
    merge_prov(FUN = sum,
                          from = "1991-01-01", to = "2015-12-31")
  expect_equal(
    mean(df$province %in%
           dictionary::province_year$`1991` %>% unique), 1)

  df <- get_gso(p_list[88]) %>%
    merge_prov(FUN = sum,
                          from = "1992-01-01", to = "2015-12-31")
  expect_equal(
    mean(df$province %in%
           dictionary::province_year$`1992` %>% unique), 1)

  df <- get_gso(p_list[88]) %>%
    merge_prov(FUN = sum,
                          from = "1997-01-01", to = "2015-12-31")
  expect_equal(
    mean(df$province %in%
           dictionary::province_year$`1997` %>% unique), 1)

  df <- get_gso(p_list[88]) %>%
    merge_prov(FUN = sum,
                          from = "2004-01-01", to = "2015-12-31")
  expect_equal(
    mean(df$province %in%
           dictionary::province_year$`2004` %>% unique), 1)

  df <- get_gso(p_list[88]) %>%
    merge_prov(FUN = sum,
                          from = "2008-01-01", to = "2015-12-31")
  expect_equal(
    mean(df$province %in%
           dictionary::province_year$`2008` %>% unique), 1)
})

#-------------------------------------------------------------------------------
test_that("`merge_prov` applies weighted mean correcty", {

  test <- get_gso(p_list[33])
  pop_size <- gso::pop_size %>% dplyr::select(province,year,total)


  # FROM >= 1992
  df <- get_gso(p_list[33]) %>%
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
              .["total"])
                         ))

  # Can Tho NA
  expect_equal(df %>%
          filter(province == "Can Tho", year == 2005) %>% .$value,
          weighted.mean(c(
            test %>% filter(province == "Can Tho", year == 2005) %>% .[,3],
            test %>% filter(province == "Hau Giang", year == 2005) %>% .[,3]),
            c(pop_size %>% filter(province == "Can Tho", year == 2005) %>%
                .["total"],
              pop_size %>% filter(province == "Hau Giang", year == 2005) %>%
                .["total"])
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
              .["total"])
                         ))

  # Yen Bai
  expect_equal(df %>%
          filter(province == "Yen Bai", year == 2004) %>% .$value,
                         weighted.mean(
          test %>% filter(province == "Yen Bai", year == 2004) %>% .[,3],
          pop_size %>% filter(province == "Yen Bai", year == 2004) %>%
            .["total"]))


  # FROM < 1992

  df <- get_gso(p_list[33]) %>%
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
              .["total"])
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
              .["total"])
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
              .["total"])
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
              .["total"]))
  )

  # From <= 1990

  df <- get_gso(p_list[33]) %>%
    merge_prov(FUN = weighted.mean,
               from = "1989-01-01", to = "2015-12-31",
               df2 = pop_size, args = "total")

  # Binh Tri Thien
  expect_equal(df %>%
          filter(province == "Binh Tri Thien", year == 2007) %>% .$value,
                         weighted.mean(c(
          test %>% filter(province == "Quang Binh", year == 2007) %>% .[,3],
          test %>% filter(province == "Quang Tri", year == 2007) %>% .[,3],
          test %>% filter(province == "Thua Thien - Hue", year == 2007) %>%
            .[,3]),
          c(pop_size %>% filter(province == "Quang Binh", year == 2007) %>%
              .["total"],
            pop_size %>% filter(province == "Quang Tri", year == 2007) %>%
              .["total"],
            pop_size %>% filter(province == "Thua Thien - Hue",
           year == 2007) %>% .["total"])
                         ))
})


# ------------------------------------------------------------------------------
test_that("`merge_prov` follow the best list", {

  test <- get_gso(p_list[88])

  # FROM >= 1992
  df <- get_gso(p_list[88]) %>%
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
  df <- get_gso(p_list[88]) %>%
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
  df <- get_gso(p_list[88]) %>%
   merge_prov(FUN = sum, diseases = c("hepatitis", "cholera", "ili"),
                          from = "1990-01-01", to = "2015-12-31")

  # Binh Tri Thien
  expect_equal(df %>%
      filter(province == "Binh Tri Thien", year == 2007, key == "total") %>%
      .$value,
      test %>% filter(province == "Quang Binh", year == 2007) %>% .$total +
      test %>% filter(province == "Quang Tri", year == 2007) %>% .$total +
      test %>% filter(province == "Thua Thien - Hue", year == 2007) %>% .$total)

  # Province
  df <- get_gso(p_list[88]) %>%
   merge_prov(FUN = sum, diseases = c("hepatitis", "cholera", "ili"),
                          from = "1990-01-01", to = "2015-12-31")
  expect_equal(
    mean(df$province %in%
           dictionary::province_year$`1979`), 1)


  df <- get_gso(p_list[88]) %>%
   merge_prov(FUN = sum, diseases = c("hepatitis", "cholera", "ili"),
                          from = "1991-01-01", to = "2015-12-31")
  expect_equal(
    mean(df$province %in%
           dictionary::province_year$`1991`), 1)

})

