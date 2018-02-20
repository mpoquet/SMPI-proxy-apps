cmake_minimum_required(VERSION 2.8.8)

project(SMPI-proxy-apps)

INCLUDE(CTest)
ENABLE_TESTING()

ADD_TEST(MDTest     bin/MDTest.sh)
ADD_TEST(MiniSMAC2D bin/stream.sh)
ADD_TEST(MiniSMAC2D bin/MiniEF.sh)
ADD_TEST(MiniSMAC2D bin/MiniSMAC2D.sh)
ADD_TEST(AMG2013    bin/AMG2013.sh)
ADD_TEST(AMG2013    bin/PSNAP.sh)
ADD_TEST(AMG2013    bin/SMB.sh)
ADD_TEST(AMG2013    bin/ZiaTest.sh)
ADD_TEST(AMG2013    bin/MiniXyce.sh)
ADD_TEST(AMG2013    bin/HPCCG.sh)
ADD_TEST(AMG2013    bin/MiniAMR.sh)

