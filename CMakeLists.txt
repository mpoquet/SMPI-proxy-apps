cmake_minimum_required(VERSION 2.8.8)

project(SMPI-proxy-apps)

INCLUDE(CTest)
ENABLE_TESTING()

ADD_TEST(MDTest     bin/MDTest.sh)
ADD_TEST(stream bin/stream.sh)
ADD_TEST(MiniEF bin/MiniEF.sh)
ADD_TEST(MiniSMAC2D bin/MiniSMAC2D.sh)
ADD_TEST(AMG2013    bin/AMG2013.sh)
ADD_TEST(PSNAP    bin/PSNAP.sh)
ADD_TEST(SMB    bin/SMB.sh)
ADD_TEST(ZiaTest    bin/ZiaTest.sh)
ADD_TEST(MiniXyce    bin/MiniXyce.sh)
ADD_TEST(HPCCG    bin/HPCCG.sh)
ADD_TEST(MiniAMR    bin/MiniAMR.sh)
ADD_TEST(CodeVault_IntegralBasic  bin/Integral_basic.sh)
ADD_TEST(CodeVault_Prime  bin/Prime.sh)
ADD_TEST(CodeVault_Pi bin/Pi.sh)
ADD_TEST(CodeVault_Prng  bin/Prng.sh)

