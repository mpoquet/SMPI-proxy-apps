cmake_minimum_required(VERSION 2.8.8)

project(SMPI-proxy-apps)

INCLUDE(CTest)
ENABLE_TESTING()

ADD_TEST(MDTest     bin/MDTest.sh)
ADD_TEST(stream bin/stream.sh)
ADD_TEST(PSNAP    bin/PSNAP.sh)
ADD_TEST(SMB    bin/SMB.sh)
ADD_TEST(ZiaTest    bin/ZiaTest.sh)

ADD_TEST(Coral_AMG2013    bin/Coral_AMG2013.sh)
ADD_TEST(Coral_HACC_IO    bin/Coral_HACC_IO.sh)

ADD_TEST(Mantevo_MiniSMAC2D bin/Mantevo_MiniSMAC2D.sh)
ADD_TEST(Mantevo_MiniEF     bin/Mantevo_MiniEF.sh)
ADD_TEST(Mantevo_MiniXyce   bin/Mantevo_MiniXyce.sh)
ADD_TEST(Mantevo_HPCCG      bin/Mantevo_HPCCG.sh)
ADD_TEST(Mantevo_MiniAMR    bin/Mantevo_MiniAMR.sh)

ADD_TEST(CodeVault_IntegralBasic  bin/CodeVault_IntegralBasic.sh)
ADD_TEST(CodeVault_Prime          bin/CodeVault_Prime.sh)
ADD_TEST(CodeVault_Pi             bin/CodeVault_Pi.sh)
ADD_TEST(CodeVault_Prng           bin/CodeVault_Prng.sh)
ADD_TEST(CodeVault_DynSparse      bin/CodeVault_DynSparse.sh)

