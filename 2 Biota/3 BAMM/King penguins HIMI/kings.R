function ()
# function to model king penguin trajectories - HIMI

{

# king penguin observations (counts as breeding pairs) at Heard Island

ObsN<-9
KingsBP<-matrix(c(1948,    3,
                  1963,   30,
                  1965,   68,
                  1969,  155,
                  1980,  900,
                  1983, 2145,
                  1986, 2931,
                  1987, 5650,
                  1992,10745),nrow=ObsN,ncol=2,byrow=TRUE)

# Mark Curran's index of MSA at January each year

MSAobsN<-155
MSA<-matrix(c(1841, 0.04,
              1842, 0.05,
              1843, 0.08,
              1844, 0.08,
              1845, 0.11,
              1846, 0.08,
              1847, 0.13,
              1848, 0.11,
              1849, 0.09,
              1850, 0.09,
              1851, 0.09,
              1852, 0.10,
              1853, 0.11,
              1854, 0.08,
              1855, 0.09,
              1856, 0.09,
              1857, 0.09,
              1858, 0.05,
              1859, 0.09,
              1860, 0.11,
              1861, 0.10,
              1862, 0.08,
              1863, 0.08,
              1864, 0.08,
              1865, 0.09,
              1866, 0.08,
              1867, 0.12,
              1868, 0.09,
              1869, 0.07,
              1870, 0.08,
              1871, 0.09,
              1872, 0.08,
              1873, 0.08,
              1874, 0.07,
              1875, 0.09,
              1876, 0.07,
              1877, 0.10,
              1878, 0.07,
              1879, 0.07,
              1880, 0.11,
              1881, 0.11,
              1882, 0.10,
              1883, 0.10,
              1884, 0.12,
              1885, 0.12,
              1886, 0.12,
              1887, 0.10,
              1888, 0.12,
              1889, 0.07,
              1890, 0.06,
              1891, 0.04,
              1892, 0.09,
              1893, 0.07,
              1894, 0.08,
              1895, 0.08,
              1896, 0.07,
              1897, 0.05,
              1898, 0.06,
              1899, 0.09,
              1900, 0.10,
              1901, 0.10,
              1902, 0.09,
              1903, 0.09,
              1904, 0.08,
              1905, 0.05,
              1906, 0.07,
              1907, 0.11,
              1908, 0.06,
              1909, 0.09,
              1910, 0.07,
              1911, 0.08,
              1912, 0.08,
              1913, 0.08,
              1914, 0.07,
              1915, 0.08,
              1916, 0.08,
              1917, 0.15,
              1918, 0.09,
              1919, 0.09,
              1920, 0.08,
              1921, 0.09,
              1922, 0.11,
              1923, 0.07,
              1924, 0.11,
              1925, 0.09,
              1926, 0.10,
              1927, 0.06,
              1928, 0.07,
              1929, 0.12,
              1930, 0.09,
              1931, 0.11,
              1932, 0.10,
              1933, 0.09,
              1934, 0.09,
              1935, 0.08,
              1936, 0.07,
              1937, 0.06,
              1938, 0.06,
              1939, 0.08,
              1940, 0.08,
              1941, 0.09,
              1942, 0.09,
              1943, 0.09,
              1944, 0.14,
              1945, 0.08,
              1946, 0.08,
              1947, 0.10,
              1948, 0.09,
              1949, 0.15,
              1950, 0.08,
              1951, 0.13,
              1952, 0.10,
              1953, 0.11,
              1954, 0.09,
              1955, 0.08,
              1956, 0.07,
              1957, 0.08,
              1958, 0.08,
              1959, 0.06,
              1960, 0.08,
              1961, 0.08,
              1962, 0.14,
              1963, 0.10,
              1964, 0.11,
              1965, 0.09,
              1966, 0.06,
              1967, 0.06,
              1968, 0.08,
              1969, 0.07,
              1970, 0.05,
              1971, 0.06,
              1972, 0.07,
              1973, 0.06,
              1974, 0.10,
              1975, 0.09,
              1976, 0.08,
              1977, 0.06,
              1978, 0.08,
              1979, 0.04,
              1980, 0.05,
              1981, 0.06,
              1982, 0.09,
              1983, 0.08,
              1984, 0.08,
              1985, 0.06,
              1986, 0.09,
              1987, 0.05,
              1988, 0.05,
              1989, 0.03,
              1990, 0.05,
              1991, 0.04,
              1992, 0.06,
              1993, 0.07,
              1994, 0.06,
              1995, 0.05),nrow=MSAobsN,ncol=2,byrow=TRUE)

MSAmean<-mean(MSA[1:120,2])
MSAnorm<-MSA[,2]/MSAmean
MSA<-cbind(MSA,MSAnorm)
plot(MSA[,1],MSA[,3],type="l")

# life cycle of King's

# Adult - breeding success contingent on condition from previous summer + winter (possibly near ice edge) (in each year, a proportion will not breed)
#       functions for year of reproduction - Reprod (adult condition)
# Hatchling - provisioning in summer 1, mortality from Skuas plus winter fast in first year
#       functions for year of reproduction - Hmort (hatchling mortality in remainder of year)
# Chick - in summer 2 - mortality contingent on provisioning and skuas
#       function - Cmort (chick mortality)
# Juvenile - at sea until age 5, possibly near ice edge for haul out during moult, giant petrel mortality
#       function - Jmort (juvenile mortality)
# Adult - summer near island, winter in open water perhaps near ice edge - mortality by leopard seals and orcas
#       function - Amort (adult mortality)


# calculate rate of increase since first observation

KingsBP<-cbind(KingsBP,KingsBP[,1])
KingsBP[1,3]<-NA
KingsBP[2:ObsN,3]<-log(KingsBP[2:ObsN,2]/KingsBP[1,2])/(KingsBP[2:ObsN,1]-KingsBP[1,1])

# calculate rate of increase from an observation since the last observation
KingsBP<-cbind(KingsBP,KingsBP[,1])
KingsBP[1,4]<-NA
for (i in 2:ObsN) {KingsBP[i,4]<-log(KingsBP[i,2]/KingsBP[(i-1),2])/(KingsBP[i,1]-KingsBP[(i-1),1])}

# determine

KingsBP
}

