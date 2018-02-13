cmake_minimum_required(VERSION 2.8.8)

project(SMPI-proxy-apps)

INCLUDE(CTest)
ENABLE_TESTING()

ADD_TEST(MDTest     bin/MDTest.sh)
ADD_TEST(AMG2013    bin/AMG2013.sh)
ADD_TEST(MiniSMAC2D bin/MiniSMAC2D.sh)
