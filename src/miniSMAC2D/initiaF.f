












c copyright notice
c                      Copyright 2013 Sandia Corporation             
c                                                                    
c     Under the terms of Contract DE-AC04-94AL85000 with Sandia      
c     Corporation, the U.S. Government retains certain rights in this
c     software.                                                      
c                                                                    
c     Redistribution and use in source and binary forms, with or     
c     without modification, are permitted provided that the following
c     conditions are met:                                            
c                                                                    
c      1. Redistributions of source code must retain the above       
c         copyright notice, this list of conditions and the following
c         disclaimer.                                                
c                                                                    
c      2. Redistributions in binary form must reproduce the above    
c         copyright notice, this list of conditions and the following
c         disclaimer in the documentation and/or other materials     
c         provided with the distribution.                            
c                                                                    
c      3. Neither the name of the Corporation nor the names of the   
c         contributors may be used to endorse or promote products    
c         derived from this software without specific prior written  
c         permission.                                                
c                                                                    
c     THIS SOFTWARE IS PROVIDED BY SANDIA CORPORATION "AS IS" AND ANY
c     EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,  
c     THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A    
c     PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL SANDIA    
c     CORPORATION OR THE CONTRIBUTORS BE LIABLE FOR ANY DIRECT,      
c     INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL     
c     DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF         
c     SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;   
c     OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF  
c     LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT      
c     (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF  
c     THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
c     SUCH DAMAGE.                                                   
c     
c
c************************************************************
c Subroutines in this file:
c  1. initia
c  2. inizone 
c
c************************************************************
c
c
c**********************************************************
      subroutine initia(geometry,title)
c**********************************************************

c  All ranks are initialized with the following variables
c
c----------------------------------------------------------
c copyright notice
c                      Copyright 2013 Sandia Corporation             
c                                                                    
c     Under the terms of Contract DE-AC04-94AL85000 with Sandia      
c     Corporation, the U.S. Government retains certain rights in this
c     software.                                                      
c                                                                    
c     Redistribution and use in source and binary forms, with or     
c     without modification, are permitted provided that the following
c     conditions are met:                                            
c                                                                    
c      1. Redistributions of source code must retain the above       
c         copyright notice, this list of conditions and the following
c         disclaimer.                                                
c                                                                    
c      2. Redistributions in binary form must reproduce the above    
c         copyright notice, this list of conditions and the following
c         disclaimer in the documentation and/or other materials     
c         provided with the distribution.                            
c                                                                    
c      3. Neither the name of the Corporation nor the names of the   
c         contributors may be used to endorse or promote products    
c         derived from this software without specific prior written  
c         permission.                                                
c                                                                    
c     THIS SOFTWARE IS PROVIDED BY SANDIA CORPORATION "AS IS" AND ANY
c     EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,  
c     THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A    
c     PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL SANDIA    
c     CORPORATION OR THE CONTRIBUTORS BE LIABLE FOR ANY DIRECT,      
c     INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL     
c     DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF         
c     SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;   
c     OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF  
c     LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT      
c     (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF  
c     THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
c     SUCH DAMAGE.                                                   
c
c====================================================================
c     
c
c copyright notice
c                      Copyright 2013 Sandia Corporation             
c                                                                    
c     Under the terms of Contract DE-AC04-94AL85000 with Sandia      
c     Corporation, the U.S. Government retains certain rights in this
c     software.                                                      
c                                                                    
c     Redistribution and use in source and binary forms, with or     
c     without modification, are permitted provided that the following
c     conditions are met:                                            
c                                                                    
c      1. Redistributions of source code must retain the above       
c         copyright notice, this list of conditions and the following
c         disclaimer.                                                
c                                                                    
c      2. Redistributions in binary form must reproduce the above    
c         copyright notice, this list of conditions and the following
c         disclaimer in the documentation and/or other materials     
c         provided with the distribution.                            
c                                                                    
c      3. Neither the name of the Corporation nor the names of the   
c         contributors may be used to endorse or promote products    
c         derived from this software without specific prior written  
c         permission.                                                
c                                                                    
c     THIS SOFTWARE IS PROVIDED BY SANDIA CORPORATION "AS IS" AND ANY
c     EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,  
c     THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A    
c     PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL SANDIA    
c     CORPORATION OR THE CONTRIBUTORS BE LIABLE FOR ANY DIRECT,      
c     INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL     
c     DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF         
c     SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;   
c     OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF  
c     LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT      
c     (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF  
c     THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
c     SUCH DAMAGE.                                                   
c
c====================================================================
c     
c









      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
!      
!      
!      (C) 2001 by Argonne National Laboratory.
!      See COPYRIGHT in top-level directory.
!      
!      DO NOT EDIT
!      This file created by buildiface 
!      
       INTEGER MPI_SOURCE, MPI_TAG, MPI_ERROR
       PARAMETER (MPI_SOURCE=3,MPI_TAG=4,MPI_ERROR=5)
       INTEGER MPI_STATUS_SIZE
       PARAMETER (MPI_STATUS_SIZE=5)
       INTEGER MPI_STATUS_IGNORE(MPI_STATUS_SIZE)
       INTEGER MPI_STATUSES_IGNORE(MPI_STATUS_SIZE,1)
       INTEGER MPI_ERRCODES_IGNORE(1)
       CHARACTER*1 MPI_ARGVS_NULL(1,1)
       CHARACTER*1 MPI_ARGV_NULL(1)
       INTEGER MPI_SUCCESS
       PARAMETER (MPI_SUCCESS=0)
       INTEGER MPI_ERR_KEYVAL
       PARAMETER (MPI_ERR_KEYVAL=48)
       INTEGER MPI_ERR_BUFFER
       PARAMETER (MPI_ERR_BUFFER=1)
       INTEGER MPI_ERR_RMA_FLAVOR
       PARAMETER (MPI_ERR_RMA_FLAVOR=58)
       INTEGER MPI_ERR_RMA_SYNC
       PARAMETER (MPI_ERR_RMA_SYNC=50)
       INTEGER MPI_ERR_FILE_EXISTS
       PARAMETER (MPI_ERR_FILE_EXISTS=25)
       INTEGER MPI_ERR_WIN
       PARAMETER (MPI_ERR_WIN=45)
       INTEGER MPI_ERR_UNKNOWN
       PARAMETER (MPI_ERR_UNKNOWN=13)
       INTEGER MPI_ERR_SERVICE
       PARAMETER (MPI_ERR_SERVICE=41)
       INTEGER MPI_ERR_BASE
       PARAMETER (MPI_ERR_BASE=46)
       INTEGER MPI_ERR_LOCKTYPE
       PARAMETER (MPI_ERR_LOCKTYPE=47)
       INTEGER MPI_ERR_INFO_VALUE
       PARAMETER (MPI_ERR_INFO_VALUE=30)
       INTEGER MPI_ERR_ASSERT
       PARAMETER (MPI_ERR_ASSERT=53)
       INTEGER MPI_ERR_TYPE
       PARAMETER (MPI_ERR_TYPE=3)
       INTEGER MPI_ERR_NO_SPACE
       PARAMETER (MPI_ERR_NO_SPACE=36)
       INTEGER MPI_ERR_INFO
       PARAMETER (MPI_ERR_INFO=28)
       INTEGER MPI_ERR_READ_ONLY
       PARAMETER (MPI_ERR_READ_ONLY=40)
       INTEGER MPI_ERR_FILE_IN_USE
       PARAMETER (MPI_ERR_FILE_IN_USE=26)
       INTEGER MPI_ERR_DISP
       PARAMETER (MPI_ERR_DISP=52)
       INTEGER MPI_ERR_ARG
       PARAMETER (MPI_ERR_ARG=12)
       INTEGER MPI_ERR_RMA_ATTACH
       PARAMETER (MPI_ERR_RMA_ATTACH=56)
       INTEGER MPI_ERR_FILE
       PARAMETER (MPI_ERR_FILE=27)
       INTEGER MPI_ERR_IN_STATUS
       PARAMETER (MPI_ERR_IN_STATUS=17)
       INTEGER MPI_ERR_OTHER
       PARAMETER (MPI_ERR_OTHER=15)
       INTEGER MPI_ERR_NOT_SAME
       PARAMETER (MPI_ERR_NOT_SAME=35)
       INTEGER MPI_ERR_CONVERSION
       PARAMETER (MPI_ERR_CONVERSION=23)
       INTEGER MPI_ERR_AMODE
       PARAMETER (MPI_ERR_AMODE=21)
       INTEGER MPI_ERR_QUOTA
       PARAMETER (MPI_ERR_QUOTA=39)
       INTEGER MPI_ERR_IO
       PARAMETER (MPI_ERR_IO=32)
       INTEGER MPI_ERR_LASTCODE
       PARAMETER (MPI_ERR_LASTCODE=1073741823)
       INTEGER MPI_ERR_RMA_RANGE
       PARAMETER (MPI_ERR_RMA_RANGE=55)
       INTEGER MPI_ERR_REQUEST
       PARAMETER (MPI_ERR_REQUEST=19)
       INTEGER MPI_ERR_DUP_DATAREP
       PARAMETER (MPI_ERR_DUP_DATAREP=24)
       INTEGER MPI_ERR_DIMS
       PARAMETER (MPI_ERR_DIMS=11)
       INTEGER MPI_ERR_INFO_NOKEY
       PARAMETER (MPI_ERR_INFO_NOKEY=31)
       INTEGER MPI_ERR_COUNT
       PARAMETER (MPI_ERR_COUNT=2)
       INTEGER MPI_ERR_UNSUPPORTED_DATAREP
       PARAMETER (MPI_ERR_UNSUPPORTED_DATAREP=43)
       INTEGER MPI_ERR_NO_MEM
       PARAMETER (MPI_ERR_NO_MEM=34)
       INTEGER MPI_ERR_TOPOLOGY
       PARAMETER (MPI_ERR_TOPOLOGY=10)
       INTEGER MPI_ERR_COMM
       PARAMETER (MPI_ERR_COMM=5)
       INTEGER MPI_ERR_GROUP
       PARAMETER (MPI_ERR_GROUP=8)
       INTEGER MPI_ERR_BAD_FILE
       PARAMETER (MPI_ERR_BAD_FILE=22)
       INTEGER MPI_ERR_TAG
       PARAMETER (MPI_ERR_TAG=4)
       INTEGER MPI_ERR_SPAWN
       PARAMETER (MPI_ERR_SPAWN=42)
       INTEGER MPI_ERR_ROOT
       PARAMETER (MPI_ERR_ROOT=7)
       INTEGER MPI_ERR_UNSUPPORTED_OPERATION
       PARAMETER (MPI_ERR_UNSUPPORTED_OPERATION=44)
       INTEGER MPI_ERR_ACCESS
       PARAMETER (MPI_ERR_ACCESS=20)
       INTEGER MPI_ERR_OP
       PARAMETER (MPI_ERR_OP=9)
       INTEGER MPI_ERR_RANK
       PARAMETER (MPI_ERR_RANK=6)
       INTEGER MPI_ERR_NAME
       PARAMETER (MPI_ERR_NAME=33)
       INTEGER MPI_ERR_RMA_SHARED
       PARAMETER (MPI_ERR_RMA_SHARED=57)
       INTEGER MPI_ERR_PORT
       PARAMETER (MPI_ERR_PORT=38)
       INTEGER MPI_ERR_NO_SUCH_FILE
       PARAMETER (MPI_ERR_NO_SUCH_FILE=37)
       INTEGER MPI_ERR_INTERN
       PARAMETER (MPI_ERR_INTERN=16)
       INTEGER MPI_ERR_INFO_KEY
       PARAMETER (MPI_ERR_INFO_KEY=29)
       INTEGER MPI_ERR_SIZE
       PARAMETER (MPI_ERR_SIZE=51)
       INTEGER MPI_ERR_TRUNCATE
       PARAMETER (MPI_ERR_TRUNCATE=14)
       INTEGER MPI_ERR_PENDING
       PARAMETER (MPI_ERR_PENDING=18)
       INTEGER MPI_ERR_RMA_CONFLICT
       PARAMETER (MPI_ERR_RMA_CONFLICT=49)
       INTEGER MPI_ERRORS_ARE_FATAL
       PARAMETER (MPI_ERRORS_ARE_FATAL=1409286144)
       INTEGER MPI_ERRORS_RETURN
       PARAMETER (MPI_ERRORS_RETURN=1409286145)
       INTEGER MPI_IDENT
       PARAMETER (MPI_IDENT=0)
       INTEGER MPI_CONGRUENT
       PARAMETER (MPI_CONGRUENT=1)
       INTEGER MPI_SIMILAR
       PARAMETER (MPI_SIMILAR=2)
       INTEGER MPI_UNEQUAL
       PARAMETER (MPI_UNEQUAL=3)
       INTEGER MPI_WIN_FLAVOR_CREATE
       PARAMETER (MPI_WIN_FLAVOR_CREATE=1)
       INTEGER MPI_WIN_FLAVOR_ALLOCATE
       PARAMETER (MPI_WIN_FLAVOR_ALLOCATE=2)
       INTEGER MPI_WIN_FLAVOR_DYNAMIC
       PARAMETER (MPI_WIN_FLAVOR_DYNAMIC=3)
       INTEGER MPI_WIN_FLAVOR_SHARED
       PARAMETER (MPI_WIN_FLAVOR_SHARED=4)
       INTEGER MPI_WIN_SEPARATE
       PARAMETER (MPI_WIN_SEPARATE=1)
       INTEGER MPI_WIN_UNIFIED
       PARAMETER (MPI_WIN_UNIFIED=2)
       INTEGER MPI_MAX
       PARAMETER (MPI_MAX=1476395009)
       INTEGER MPI_MIN
       PARAMETER (MPI_MIN=1476395010)
       INTEGER MPI_SUM
       PARAMETER (MPI_SUM=1476395011)
       INTEGER MPI_PROD
       PARAMETER (MPI_PROD=1476395012)
       INTEGER MPI_LAND
       PARAMETER (MPI_LAND=1476395013)
       INTEGER MPI_BAND
       PARAMETER (MPI_BAND=1476395014)
       INTEGER MPI_LOR
       PARAMETER (MPI_LOR=1476395015)
       INTEGER MPI_BOR
       PARAMETER (MPI_BOR=1476395016)
       INTEGER MPI_LXOR
       PARAMETER (MPI_LXOR=1476395017)
       INTEGER MPI_BXOR
       PARAMETER (MPI_BXOR=1476395018)
       INTEGER MPI_MINLOC
       PARAMETER (MPI_MINLOC=1476395019)
       INTEGER MPI_MAXLOC
       PARAMETER (MPI_MAXLOC=1476395020)
       INTEGER MPI_REPLACE
       PARAMETER (MPI_REPLACE=1476395021)
       INTEGER MPI_NO_OP
       PARAMETER (MPI_NO_OP=1476395022)
       INTEGER MPI_COMM_WORLD
       PARAMETER (MPI_COMM_WORLD=1140850688)
       INTEGER MPI_COMM_SELF
       PARAMETER (MPI_COMM_SELF=1140850689)
       INTEGER MPI_GROUP_EMPTY
       PARAMETER (MPI_GROUP_EMPTY=1207959552)
       INTEGER MPI_COMM_NULL
       PARAMETER (MPI_COMM_NULL=67108864)
       INTEGER MPI_WIN_NULL
       PARAMETER (MPI_WIN_NULL=536870912)
       INTEGER MPI_FILE_NULL
       PARAMETER (MPI_FILE_NULL=0)
       INTEGER MPI_GROUP_NULL
       PARAMETER (MPI_GROUP_NULL=134217728)
       INTEGER MPI_OP_NULL
       PARAMETER (MPI_OP_NULL=402653184)
       INTEGER MPI_DATATYPE_NULL
       PARAMETER (MPI_DATATYPE_NULL=201326592)
       INTEGER MPI_REQUEST_NULL
       PARAMETER (MPI_REQUEST_NULL=738197504)
       INTEGER MPI_ERRHANDLER_NULL
       PARAMETER (MPI_ERRHANDLER_NULL=335544320)
       INTEGER MPI_INFO_NULL
       PARAMETER (MPI_INFO_NULL=469762048)
       INTEGER MPI_INFO_ENV
       PARAMETER (MPI_INFO_ENV=1543503873)
       INTEGER MPI_TAG_UB
       PARAMETER (MPI_TAG_UB=1681915906)
       INTEGER MPI_HOST
       PARAMETER (MPI_HOST=1681915908)
       INTEGER MPI_IO
       PARAMETER (MPI_IO=1681915910)
       INTEGER MPI_WTIME_IS_GLOBAL
       PARAMETER (MPI_WTIME_IS_GLOBAL=1681915912)
       INTEGER MPI_UNIVERSE_SIZE
       PARAMETER (MPI_UNIVERSE_SIZE=1681915914)
       INTEGER MPI_LASTUSEDCODE
       PARAMETER (MPI_LASTUSEDCODE=1681915916)
       INTEGER MPI_APPNUM
       PARAMETER (MPI_APPNUM=1681915918)
       INTEGER MPI_WIN_BASE
       PARAMETER (MPI_WIN_BASE=1711276034)
       INTEGER MPI_WIN_SIZE
       PARAMETER (MPI_WIN_SIZE=1711276036)
       INTEGER MPI_WIN_DISP_UNIT
       PARAMETER (MPI_WIN_DISP_UNIT=1711276038)
       INTEGER MPI_WIN_CREATE_FLAVOR
       PARAMETER (MPI_WIN_CREATE_FLAVOR=1711276040)
       INTEGER MPI_WIN_MODEL
       PARAMETER (MPI_WIN_MODEL=1711276042)
       INTEGER MPI_MAX_ERROR_STRING
       PARAMETER (MPI_MAX_ERROR_STRING=512-1)
       INTEGER MPI_MAX_PORT_NAME
       PARAMETER (MPI_MAX_PORT_NAME=255)
       INTEGER MPI_MAX_OBJECT_NAME
       PARAMETER (MPI_MAX_OBJECT_NAME=127)
       INTEGER MPI_MAX_INFO_KEY
       PARAMETER (MPI_MAX_INFO_KEY=254)
       INTEGER MPI_MAX_INFO_VAL
       PARAMETER (MPI_MAX_INFO_VAL=1023)
       INTEGER MPI_MAX_PROCESSOR_NAME
       PARAMETER (MPI_MAX_PROCESSOR_NAME=128-1)
       INTEGER MPI_MAX_DATAREP_STRING
       PARAMETER (MPI_MAX_DATAREP_STRING=127)
       INTEGER MPI_MAX_LIBRARY_VERSION_STRING
       PARAMETER (MPI_MAX_LIBRARY_VERSION_STRING=8192-1)
       INTEGER MPI_UNDEFINED
       PARAMETER (MPI_UNDEFINED=(-32766))
       INTEGER MPI_KEYVAL_INVALID
       PARAMETER (MPI_KEYVAL_INVALID=603979776)
       INTEGER MPI_BSEND_OVERHEAD
       PARAMETER (MPI_BSEND_OVERHEAD=96)
       INTEGER MPI_PROC_NULL
       PARAMETER (MPI_PROC_NULL=-1)
       INTEGER MPI_ANY_SOURCE
       PARAMETER (MPI_ANY_SOURCE=-2)
       INTEGER MPI_ANY_TAG
       PARAMETER (MPI_ANY_TAG=-1)
       INTEGER MPI_ROOT
       PARAMETER (MPI_ROOT=-3)
       INTEGER MPI_GRAPH
       PARAMETER (MPI_GRAPH=1)
       INTEGER MPI_CART
       PARAMETER (MPI_CART=2)
       INTEGER MPI_DIST_GRAPH
       PARAMETER (MPI_DIST_GRAPH=3)
       INTEGER MPI_VERSION
       PARAMETER (MPI_VERSION=3)
       INTEGER MPI_SUBVERSION
       PARAMETER (MPI_SUBVERSION=1)
       INTEGER MPI_LOCK_EXCLUSIVE
       PARAMETER (MPI_LOCK_EXCLUSIVE=234)
       INTEGER MPI_LOCK_SHARED
       PARAMETER (MPI_LOCK_SHARED=235)
       INTEGER MPI_COMPLEX
       PARAMETER (MPI_COMPLEX=1275070494)
       INTEGER MPI_DOUBLE_COMPLEX
       PARAMETER (MPI_DOUBLE_COMPLEX=1275072546)
       INTEGER MPI_LOGICAL
       PARAMETER (MPI_LOGICAL=1275069469)
       INTEGER MPI_REAL
       PARAMETER (MPI_REAL=1275069468)
       INTEGER MPI_DOUBLE_PRECISION
       PARAMETER (MPI_DOUBLE_PRECISION=1275070495)
       INTEGER MPI_INTEGER
       PARAMETER (MPI_INTEGER=1275069467)
       INTEGER MPI_2INTEGER
       PARAMETER (MPI_2INTEGER=1275070496)
       INTEGER MPI_2DOUBLE_PRECISION
       PARAMETER (MPI_2DOUBLE_PRECISION=1275072547)
       INTEGER MPI_2REAL
       PARAMETER (MPI_2REAL=1275070497)
       INTEGER MPI_CHARACTER
       PARAMETER (MPI_CHARACTER=1275068698)
       INTEGER MPI_BYTE
       PARAMETER (MPI_BYTE=1275068685)
       INTEGER MPI_UB
       PARAMETER (MPI_UB=1275068433)
       INTEGER MPI_LB
       PARAMETER (MPI_LB=1275068432)
       INTEGER MPI_PACKED
       PARAMETER (MPI_PACKED=1275068687)
       INTEGER MPI_INTEGER1
       PARAMETER (MPI_INTEGER1=1275068717)
       INTEGER MPI_INTEGER2
       PARAMETER (MPI_INTEGER2=1275068975)
       INTEGER MPI_INTEGER4
       PARAMETER (MPI_INTEGER4=1275069488)
       INTEGER MPI_INTEGER8
       PARAMETER (MPI_INTEGER8=1275070513)
       INTEGER MPI_INTEGER16
       PARAMETER (MPI_INTEGER16=MPI_DATATYPE_NULL)
       INTEGER MPI_REAL4
       PARAMETER (MPI_REAL4=1275069479)
       INTEGER MPI_REAL8
       PARAMETER (MPI_REAL8=1275070505)
       INTEGER MPI_REAL16
       PARAMETER (MPI_REAL16=1275072555)
       INTEGER MPI_COMPLEX8
       PARAMETER (MPI_COMPLEX8=1275070504)
       INTEGER MPI_COMPLEX16
       PARAMETER (MPI_COMPLEX16=1275072554)
       INTEGER MPI_COMPLEX32
       PARAMETER (MPI_COMPLEX32=1275076652)
       INTEGER MPI_ADDRESS_KIND
       PARAMETER (MPI_ADDRESS_KIND=8)
       INTEGER MPI_OFFSET_KIND
       PARAMETER (MPI_OFFSET_KIND=8)
       INTEGER MPI_COUNT_KIND
       PARAMETER (MPI_COUNT_KIND=8)
       INTEGER MPI_INTEGER_KIND
       PARAMETER (MPI_INTEGER_KIND=4)
       INTEGER MPI_CHAR
       PARAMETER (MPI_CHAR=1275068673)
       INTEGER MPI_SIGNED_CHAR
       PARAMETER (MPI_SIGNED_CHAR=1275068696)
       INTEGER MPI_UNSIGNED_CHAR
       PARAMETER (MPI_UNSIGNED_CHAR=1275068674)
       INTEGER MPI_WCHAR
       PARAMETER (MPI_WCHAR=1275069454)
       INTEGER MPI_SHORT
       PARAMETER (MPI_SHORT=1275068931)
       INTEGER MPI_UNSIGNED_SHORT
       PARAMETER (MPI_UNSIGNED_SHORT=1275068932)
       INTEGER MPI_INT
       PARAMETER (MPI_INT=1275069445)
       INTEGER MPI_UNSIGNED
       PARAMETER (MPI_UNSIGNED=1275069446)
       INTEGER MPI_LONG
       PARAMETER (MPI_LONG=1275070471)
       INTEGER MPI_UNSIGNED_LONG
       PARAMETER (MPI_UNSIGNED_LONG=1275070472)
       INTEGER MPI_FLOAT
       PARAMETER (MPI_FLOAT=1275069450)
       INTEGER MPI_DOUBLE
       PARAMETER (MPI_DOUBLE=1275070475)
       INTEGER MPI_LONG_DOUBLE
       PARAMETER (MPI_LONG_DOUBLE=1275072524)
       INTEGER MPI_LONG_LONG_INT
       PARAMETER (MPI_LONG_LONG_INT=1275070473)
       INTEGER MPI_UNSIGNED_LONG_LONG
       PARAMETER (MPI_UNSIGNED_LONG_LONG=1275070489)
       INTEGER MPI_LONG_LONG
       PARAMETER (MPI_LONG_LONG=1275070473)
       INTEGER MPI_FLOAT_INT
       PARAMETER (MPI_FLOAT_INT=-1946157056)
       INTEGER MPI_DOUBLE_INT
       PARAMETER (MPI_DOUBLE_INT=-1946157055)
       INTEGER MPI_LONG_INT
       PARAMETER (MPI_LONG_INT=-1946157054)
       INTEGER MPI_SHORT_INT
       PARAMETER (MPI_SHORT_INT=-1946157053)
       INTEGER MPI_2INT
       PARAMETER (MPI_2INT=1275070486)
       INTEGER MPI_LONG_DOUBLE_INT
       PARAMETER (MPI_LONG_DOUBLE_INT=-1946157052)
       INTEGER MPI_INT8_T
       PARAMETER (MPI_INT8_T=1275068727)
       INTEGER MPI_INT16_T
       PARAMETER (MPI_INT16_T=1275068984)
       INTEGER MPI_INT32_T
       PARAMETER (MPI_INT32_T=1275069497)
       INTEGER MPI_INT64_T
       PARAMETER (MPI_INT64_T=1275070522)
       INTEGER MPI_UINT8_T
       PARAMETER (MPI_UINT8_T=1275068731)
       INTEGER MPI_UINT16_T
       PARAMETER (MPI_UINT16_T=1275068988)
       INTEGER MPI_UINT32_T
       PARAMETER (MPI_UINT32_T=1275069501)
       INTEGER MPI_UINT64_T
       PARAMETER (MPI_UINT64_T=1275070526)
       INTEGER MPI_C_BOOL
       PARAMETER (MPI_C_BOOL=1275068735)
       INTEGER MPI_C_FLOAT_COMPLEX
       PARAMETER (MPI_C_FLOAT_COMPLEX=1275070528)
       INTEGER MPI_C_COMPLEX
       PARAMETER (MPI_C_COMPLEX=1275070528)
       INTEGER MPI_C_DOUBLE_COMPLEX
       PARAMETER (MPI_C_DOUBLE_COMPLEX=1275072577)
       INTEGER MPI_C_LONG_DOUBLE_COMPLEX
       PARAMETER (MPI_C_LONG_DOUBLE_COMPLEX=1275076674)
       INTEGER MPI_AINT
       PARAMETER (MPI_AINT=1275070531)
       INTEGER MPI_OFFSET
       PARAMETER (MPI_OFFSET=1275070532)
       INTEGER MPI_COUNT
       PARAMETER (MPI_COUNT=1275070533)
       INTEGER MPI_CXX_BOOL
       PARAMETER (MPI_CXX_BOOL=1275068723)
       INTEGER MPI_CXX_FLOAT_COMPLEX
       PARAMETER (MPI_CXX_FLOAT_COMPLEX=1275070516)
       INTEGER MPI_CXX_DOUBLE_COMPLEX
       PARAMETER (MPI_CXX_DOUBLE_COMPLEX=1275072565)
       INTEGER MPI_CXX_LONG_DOUBLE_COMPLEX
       PARAMETER (MPI_CXX_LONG_DOUBLE_COMPLEX=1275076662)
       INTEGER MPI_COMBINER_NAMED
       PARAMETER (MPI_COMBINER_NAMED=1)
       INTEGER MPI_COMBINER_DUP
       PARAMETER (MPI_COMBINER_DUP=2)
       INTEGER MPI_COMBINER_CONTIGUOUS
       PARAMETER (MPI_COMBINER_CONTIGUOUS=3)
       INTEGER MPI_COMBINER_VECTOR
       PARAMETER (MPI_COMBINER_VECTOR=4)
       INTEGER MPI_COMBINER_HVECTOR_INTEGER
       PARAMETER (MPI_COMBINER_HVECTOR_INTEGER=5)
       INTEGER MPI_COMBINER_HVECTOR
       PARAMETER (MPI_COMBINER_HVECTOR=6)
       INTEGER MPI_COMBINER_INDEXED
       PARAMETER (MPI_COMBINER_INDEXED=7)
       INTEGER MPI_COMBINER_HINDEXED_INTEGER
       PARAMETER (MPI_COMBINER_HINDEXED_INTEGER=8)
       INTEGER MPI_COMBINER_HINDEXED
       PARAMETER (MPI_COMBINER_HINDEXED=9)
       INTEGER MPI_COMBINER_INDEXED_BLOCK
       PARAMETER (MPI_COMBINER_INDEXED_BLOCK=10)
       INTEGER MPI_COMBINER_STRUCT_INTEGER
       PARAMETER (MPI_COMBINER_STRUCT_INTEGER=11)
       INTEGER MPI_COMBINER_STRUCT
       PARAMETER (MPI_COMBINER_STRUCT=12)
       INTEGER MPI_COMBINER_SUBARRAY
       PARAMETER (MPI_COMBINER_SUBARRAY=13)
       INTEGER MPI_COMBINER_DARRAY
       PARAMETER (MPI_COMBINER_DARRAY=14)
       INTEGER MPI_COMBINER_F90_REAL
       PARAMETER (MPI_COMBINER_F90_REAL=15)
       INTEGER MPI_COMBINER_F90_COMPLEX
       PARAMETER (MPI_COMBINER_F90_COMPLEX=16)
       INTEGER MPI_COMBINER_F90_INTEGER
       PARAMETER (MPI_COMBINER_F90_INTEGER=17)
       INTEGER MPI_COMBINER_RESIZED
       PARAMETER (MPI_COMBINER_RESIZED=18)
       INTEGER MPI_COMBINER_HINDEXED_BLOCK
       PARAMETER (MPI_COMBINER_HINDEXED_BLOCK=19)
       INTEGER MPI_TYPECLASS_REAL
       PARAMETER (MPI_TYPECLASS_REAL=1)
       INTEGER MPI_TYPECLASS_INTEGER
       PARAMETER (MPI_TYPECLASS_INTEGER=2)
       INTEGER MPI_TYPECLASS_COMPLEX
       PARAMETER (MPI_TYPECLASS_COMPLEX=3)
       INTEGER MPI_MODE_NOCHECK
       PARAMETER (MPI_MODE_NOCHECK=1024)
       INTEGER MPI_MODE_NOSTORE
       PARAMETER (MPI_MODE_NOSTORE=2048)
       INTEGER MPI_MODE_NOPUT
       PARAMETER (MPI_MODE_NOPUT=4096)
       INTEGER MPI_MODE_NOPRECEDE
       PARAMETER (MPI_MODE_NOPRECEDE=8192)
       INTEGER MPI_MODE_NOSUCCEED
       PARAMETER (MPI_MODE_NOSUCCEED=16384)
       INTEGER MPI_COMM_TYPE_SHARED
       PARAMETER (MPI_COMM_TYPE_SHARED=1)
       INTEGER MPI_MESSAGE_NULL
       PARAMETER (MPI_MESSAGE_NULL=738197504)
       INTEGER MPI_MESSAGE_NO_PROC
       PARAMETER (MPI_MESSAGE_NO_PROC=1811939328)
       INTEGER MPI_THREAD_SINGLE
       PARAMETER (MPI_THREAD_SINGLE=0)
       INTEGER MPI_THREAD_FUNNELED
       PARAMETER (MPI_THREAD_FUNNELED=1)
       INTEGER MPI_THREAD_SERIALIZED
       PARAMETER (MPI_THREAD_SERIALIZED=2)
       INTEGER MPI_THREAD_MULTIPLE
       PARAMETER (MPI_THREAD_MULTIPLE=3)
       INTEGER MPI_MODE_RDONLY
       PARAMETER (MPI_MODE_RDONLY=2)
       INTEGER MPI_MODE_RDWR
       PARAMETER (MPI_MODE_RDWR=8)
       INTEGER MPI_MODE_WRONLY
       PARAMETER (MPI_MODE_WRONLY=4)
       INTEGER MPI_MODE_DELETE_ON_CLOSE
       PARAMETER (MPI_MODE_DELETE_ON_CLOSE=16)
       INTEGER MPI_MODE_UNIQUE_OPEN
       PARAMETER (MPI_MODE_UNIQUE_OPEN=32)
       INTEGER MPI_MODE_CREATE
       PARAMETER (MPI_MODE_CREATE=1)
       INTEGER MPI_MODE_EXCL
       PARAMETER (MPI_MODE_EXCL=64)
       INTEGER MPI_MODE_APPEND
       PARAMETER (MPI_MODE_APPEND=128)
       INTEGER MPI_MODE_SEQUENTIAL
       PARAMETER (MPI_MODE_SEQUENTIAL=256)
       INTEGER MPI_SEEK_SET
       PARAMETER (MPI_SEEK_SET=600)
       INTEGER MPI_SEEK_CUR
       PARAMETER (MPI_SEEK_CUR=602)
       INTEGER MPI_SEEK_END
       PARAMETER (MPI_SEEK_END=604)
       INTEGER MPI_ORDER_C
       PARAMETER (MPI_ORDER_C=56)
       INTEGER MPI_ORDER_FORTRAN
       PARAMETER (MPI_ORDER_FORTRAN=57)
       INTEGER MPI_DISTRIBUTE_BLOCK
       PARAMETER (MPI_DISTRIBUTE_BLOCK=121)
       INTEGER MPI_DISTRIBUTE_CYCLIC
       PARAMETER (MPI_DISTRIBUTE_CYCLIC=122)
       INTEGER MPI_DISTRIBUTE_NONE
       PARAMETER (MPI_DISTRIBUTE_NONE=123)
       INTEGER MPI_DISTRIBUTE_DFLT_DARG
       PARAMETER (MPI_DISTRIBUTE_DFLT_DARG=-49767)
       integer*8 MPI_DISPLACEMENT_CURRENT
       PARAMETER (MPI_DISPLACEMENT_CURRENT=-54278278)
       LOGICAL MPI_SUBARRAYS_SUPPORTED
       PARAMETER(MPI_SUBARRAYS_SUPPORTED=.FALSE.)
       LOGICAL MPI_ASYNC_PROTECTS_NONBLOCKING
       PARAMETER(MPI_ASYNC_PROTECTS_NONBLOCKING=.FALSE.)
       INTEGER MPI_BOTTOM, MPI_IN_PLACE, MPI_UNWEIGHTED
       INTEGER MPI_WEIGHTS_EMPTY
       EXTERNAL MPI_DUP_FN, MPI_NULL_DELETE_FN, MPI_NULL_COPY_FN
       EXTERNAL MPI_WTIME, MPI_WTICK
       EXTERNAL PMPI_WTIME, PMPI_WTICK
       EXTERNAL MPI_COMM_DUP_FN, MPI_COMM_NULL_DELETE_FN
       EXTERNAL MPI_COMM_NULL_COPY_FN
       EXTERNAL MPI_WIN_DUP_FN, MPI_WIN_NULL_DELETE_FN
       EXTERNAL MPI_WIN_NULL_COPY_FN
       EXTERNAL MPI_TYPE_DUP_FN, MPI_TYPE_NULL_DELETE_FN
       EXTERNAL MPI_TYPE_NULL_COPY_FN
       EXTERNAL MPI_CONVERSION_FN_NULL
       REAL*8 MPI_WTIME, MPI_WTICK
       REAL*8 PMPI_WTIME, PMPI_WTICK


       COMMON /MPIFCMB5/ MPI_UNWEIGHTED
       COMMON /MPIFCMB9/ MPI_WEIGHTS_EMPTY
       SAVE /MPIFCMB5/
       SAVE /MPIFCMB9/

       COMMON /MPIPRIV1/ MPI_BOTTOM, MPI_IN_PLACE, MPI_STATUS_IGNORE

       COMMON /MPIPRIV2/ MPI_STATUSES_IGNORE, MPI_ERRCODES_IGNORE
       SAVE /MPIPRIV1/,/MPIPRIV2/

       COMMON /MPIPRIVC/ MPI_ARGVS_NULL, MPI_ARGV_NULL
       SAVE   /MPIPRIVC/
c copyright notice
c                      Copyright 2013 Sandia Corporation             
c                                                                    
c     Under the terms of Contract DE-AC04-94AL85000 with Sandia      
c     Corporation, the U.S. Government retains certain rights in this
c     software.                                                      
c                                                                    
c     Redistribution and use in source and binary forms, with or     
c     without modification, are permitted provided that the following
c     conditions are met:                                            
c                                                                    
c      1. Redistributions of source code must retain the above       
c         copyright notice, this list of conditions and the following
c         disclaimer.                                                
c                                                                    
c      2. Redistributions in binary form must reproduce the above    
c         copyright notice, this list of conditions and the following
c         disclaimer in the documentation and/or other materials     
c         provided with the distribution.                            
c                                                                    
c      3. Neither the name of the Corporation nor the names of the   
c         contributors may be used to endorse or promote products    
c         derived from this software without specific prior written  
c         permission.                                                
c                                                                    
c     THIS SOFTWARE IS PROVIDED BY SANDIA CORPORATION "AS IS" AND ANY
c     EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,  
c     THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A    
c     PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL SANDIA    
c     CORPORATION OR THE CONTRIBUTORS BE LIABLE FOR ANY DIRECT,      
c     INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL     
c     DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF         
c     SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;   
c     OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF  
c     LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT      
c     (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF  
c     THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
c     SUCH DAMAGE.                                                   
c
c====================================================================
c     
c
      common/mpi_params/nodeid,numprocs
c
      common/impli/impsch,njsp,nksp,nbcimp,ntjsp,ntksp
      common/mass/pin,pout
      common/timm/beta,dtau,dt,time
      common/visc/reynum,vnu
      common/iparmi/nt,niter,ntmax,ntime,iflxo,ivis,iturb
      common/airfr/alpha,clift,cdrag,cmom
      common/implr/underr,dcoef2,dcoef4
      common/unitno/istdout
      common/gridmods/kadd
      common/write_p3d_q/write_plot3d_q_file
      common/bcmain_location/bcmain_directory,bcmain_filename
      common/xygrid_location/xygrid_directory,xygrid_filename

      character*60 title,geometry, namelist_input_file
      character*100 bcmain_directory,bcmain_filename
      character*100 xygrid_directory,xygrid_filename

      logical DEBUG
      logical zz_run_default_input
      logical write_plot3d_q_file
      double precision, parameter :: pi = 4.d0*datan(1.d0)
      integer nodeid,numprocs,ierr,stat(MPI_STATUS_SIZE)
      REAL*8 rvar(6)
      INTEGER ivar(4)

      namelist/input/
     &  alpha,beta,dcoef2,dcoef4,dt,dtau,geometry,impsch,iturb,ivis,
     &  kadd,ntmax,reynum,title,underr,zz_run_default_input,
     &  write_plot3d_q_file,bcmain_directory,bcmain_filename,
     &  xygrid_directory,xygrid_filename
c********************************************************

      DEBUG = .false.
c      DEBUG = .true.

      if (DEBUG) then
       print*
       print*, ' >>> Entering sub initia - node = ',nodeid,' <<<'
       print*, '   nodeid = ',nodeid
       print*, '   numprocs = ',numprocs
      endif

c Default values for select variables 
c ... ints
      nbcimp = 1        ! line relaxation sweep implicit bc update frequency
      niter = 1         ! output at every niter iterations
      istdout = 6       ! standard output handle
c ... reals
      pin = 1.5D0
      pout = 1.0D0
c ... number of grid points added to outer boundary; used for increasing the distance
c     of the grid outer boundary and for scaling studies
      kadd=0

c Read input namelist from file
c-------------------------------
      if (nodeid.eq.0) then
c-------------------------------
       namelist_input_file = 'smac2d.in'
       open (unit=900,file=namelist_input_file,err=998)
       goto 999
998    continue
       write(istdout,11) namelist_input_file
       write(*,11) namelist_input_file
11     format(/'ERROR: namelist input file not found:',/,
     & '   file = ',a,/,
     & '    Program terminating.'/)
       call MPI_ABORT(MPI_COMM_WORLD,1)
       stop 'stopping: namelist input file not found'

999    continue

       read(900,input,err=991)
       goto 992
991    continue
       write(istdout,12) namelist_input_file
       write(*,12) namelist_input_file
12     format(/'ERROR: cannot read from namelist input file:',/,
     &  '   file = ',a,/,
     &  '    Program terminating.'/)
       call MPI_ABORT(MPI_COMM_WORLD,1)
       stop 'stopping: cannot read from namelist input file'

992    continue
       print*
       print*, 'From namelist file:',namelist_input_file
       print*, '-----------------------------------------------'
       write(istdout,input)
       close(900)

c DEFAULT VALUES SPECIFIC TO AIRFOIL (airfoil)
c-------------------
       if (zz_run_default_input) then
c-------------------
c ... use default values for airfoil
        alpha = 8.0d0             ! degs, airfoil angle of attack
        beta = 5.D0               ! artificial compressibility factor
        dcoef2 = 0.00d0		  ! 2nd-order smoothing coefficient
        dcoef4 = 0.00d0           ! 4th-order smoothing coefficient
        dt = 1.0d12               ! time step for turbulence model
        dtau=0.1d0                ! pseudo-time step for steady state case 
        geometry='airfoil'        ! geometry under consideration
        impsch = 1                ! implicit scheme, 1=line relaxation; 2=lusgs
        iturb = 1		  ! use Spallart-Allmaras turbulence model
        ivis = 1                  ! variable viscosity and non-orthogonal grid
        kadd=0                    ! add layers (points) to grid outer boundary
        ntmax = 500               ! number of time steps to take
        reynum = 1.50D6           ! reynolds number of 1,500,000
        title='SMAC2D -- Sandia MiniAero Code 2D - DEFAULT VALUES' !title
        underr = 0.01D0           ! under-relaxation parameter, 0 < underr < 1
        write_plot3d_q_file=.false. ! do not write plot3d q file, may be big
        bcmain_directory = '.'
        bcmain_filename = 'bcmain.dat_original_7kx7k'
        xygrid_directory = '.'
        xygrid_filename = 'xy.dat_original_7kx7k'
        print*
        print*, 'DEFAULT AIRFOIL VALUES ARE BEING USED FOR THIS RUN:'
        if (DEBUG) then
         write(*,955) alpha,beta,dt,dtau,trim(geometry),impsch,iturb,
     &    ivis,ntmax,reynum,trim(title),underr,write_plot3d_q_file,
     &    bcmain_directory,bcmain_filename,
     &    xygrid_directory,xygrid_filename
955      format('alpha = ',f6.3,/,
     &     'beta = ',f6.3,/,
     &     'dt = ',1pd14.7,/,
     &     'dtau = ',1pd14.7,/,
     &     'geometry = ',a,/,
     &     'impsch = ',i1,/,
     &     'iturb = ',i1,/,
     &     'ivis = ',i1,/,
     &     'kadd = ',i10,/,
     &     'ntmax = ',i7,/,
     &     'reynum = ',1pd14.7,/,
     &     'title = ',a,/,
     &     'underr = ',1pd11.4,/,
     &     'write_plot3d_q_file = ',l2,/,
     &     'bcmain_directory = ',a,/,
     &     'bcmain_filename = ',a,/,
     &     'xygrid_directory = ',a,/,
     &     'xygrid_filename = ',a,/
     &     )
       endif
      endif
c a few checks on input values
c ... ivis
      if (ivis.ne.0.and.ivis.ne.1) then
       print*
       print*, 'ERROR: invalid value for ivis on nodeid =',nodeid
       print*, '  ivis must be either 0 (inviscid) or 1 (viscous)'
       print*, '  Current value of ivis =',ivis
       print*, 'Program terminating.'
       print*
       call MPI_ABORT(MPI_COMM_WORLD,1)
       stop 'stop: invalid value for ivis'
      endif
c ... iturb
      if (iturb.ne.0.and.iturb.ne.1) then
       print*
       print*, 'ERROR: invalid value for iturb on nodeid =',nodeid
       print*, '  iturb must be either 0 (laminar) or 1 (turbulent)'
       print*, '  Current value of iturb =',iturb
       print*, 'Program terminating.'
       print*
       call MPI_ABORT(MPI_COMM_WORLD,1)
       stop 'stop: invalid value for iturb'
      endif
c ... kadd
      if (kadd.lt.0) then
       print*
       print*, 'ERROR: invalid value for kadd on nodeid =',nodeid
       print*, '  kadd must be equal to or greater than zero.'
       print*, '  Current value of kadd =',kadd
       print*, 'Program terminating.'
       print*
       call MPI_ABORT(MPI_COMM_WORLD,1)
       stop 'stop: invalid value for kadd'
      endif
c ... underr
      if (underr.le.0.0d0.or.underr.gt.1.0d0) then
       print*
       print*, ' ERROR: invalid value for underr on nodeid =',nodeid
       print*, '   underr must be between 0 and 1'
       print*, '   Current value of underr =',underr
       print*, 'Program terminating.'
       print*
       call MPI_ABORT(MPI_COMM_WORLD,1)
       stop 'stop: invalid value for underr'
      endif
c Send values to other nodes
c ... pack the namelist input variables
c ...   integers
      ivar(1) = impsch
      ivar(2) = iturb
      ivar(3) = ivis
      ivar(4) = ntmax
c ...   reals
      rvar(1) = alpha
      rvar(2) = beta
      rvar(3) = dt
      rvar(4) = dtau
      rvar(5) = reynum
      rvar(6) = underr
c ... send integers
      do 888 i=2,numprocs
       node_out = i-1
c ... send integers
       call MPI_SEND(ivar,4,MPI_INTEGER,node_out,node_out,
     &  MPI_COMM_WORLD,ierr)

c ... send reals
       call MPI_SEND(rvar,6,MPI_REAL8,node_out,node_out+1,
     &  MPI_COMM_WORLD,ierr)

888   continue

c-------------------
      else
c-------------------
c for nodes other than node 0
c ... receive integers
       call MPI_RECV(ivar,4,MPI_INTEGER,0,nodeid,
     &  MPI_COMM_WORLD,stat,ierr)
c ...   unpack integers
       impsch = ivar(1)
       iturb = ivar(2)
       ivis = ivar(3)
       ntmax = ivar(4)

c ... receive reals
       call MPI_RECV(rvar,6,MPI_REAL8,0,nodeid+1,
     &  MPI_COMM_WORLD,stat,ierr)

c ...   unpack reals
       alpha = rvar(1)
       beta = rvar(2)
       dt = rvar(3)
       dtau = rvar(4)
       reynum = rvar(5)
       underr = rvar(6)
c-------------------
      endif
c-------------------

      if (DEBUG) then
       call flush(6)
       call flush(istdout)
c       call MPI_ABORT(MPI_COMM_WORLD,1)
c       stop 'stop: after reading namelist input file'
      endif

c-----
c  Initialize following variables on all nodes
c-----
c ... kinemative viscosity
      vnu = 1.0D0/reynum
c ... initial conditions
      ntime = 0
      nt = 0
      time = 0.0
      clift = 0.0

c-----
c  Print initialized variables
c-----
c********************
      if (nodeid.eq.0) then
c********************
       print*
       print*, '+++++++++++++++++++++++++++++++++++++++++++++++++++++'
       print*, '       Flow Conditions and Initialized Variables     '
       print*, '+++++++++++++++++++++++++++++++++++++++++++++++++++++'
       write(*,550) title
550    format(' title: ',a)
       write(*,555) geometry
555    format(' geometry: ',a)
       print*, 'nbcimp: ',nbcimp
       print*, 'niter: ',niter
       print*, 'ntmax: ',ntmax
       print*, 'impsch: ',impsch
       print*, 'iturb: ',iturb
       print*, 'ivis: ',ivis
       print*
       print*, 'fp_precision: double'
       print*
       print*, 'beta: ',beta
       print*, 'dtau: ',dtau
       print*, 'dt: ',dt
       print*
       print*, 'kadd: ',kadd
       print*
       print*, 'pin: ',pin
       print*, 'pout: ',pout
       print*, 'reynum: ',reynum
       print*, 'underr: ',underr
       print*, 'vnu = 1./reynum: ',vnu
       print*, 'write_plot3d_q_file: ',write_plot3d_q_file
       print*, 'bcmain_directory: ',bcmain_directory
       print*, 'bcmain_filename: ',bcmain_filename
       print*, 'xygrid_directory: ',xygrid_directory
       print*, 'xygrid_filename: ',xygrid_filename
       print*, '+++++++++++++++++++++++++++++++++++++++++++++++++++++'
       print*
c********************
      endif
c********************

       call MPI_BARRIER(MPI_COMM_WORLD,ierr)

      if (DEBUG) then
       print*
       print*, ' Finished in initia.f/initia 100 - node =',nodeid
       call flush(6)
       print*, ' This is nodeid =',nodeid
       call MPI_ABORT(MPI_COMM_WORLD,1)
       stop 'stop: at end of initia.f/initia'
      endif

c-----
c  End of initia
c-----
      return
      end
c
c
c***********************************************************************
      subroutine inizone
c***********************************************************************
c  Initializes variables specific to one particular zone.
c
c------------------------------------------------------------------
c copyright notice
c                      Copyright 2013 Sandia Corporation             
c                                                                    
c     Under the terms of Contract DE-AC04-94AL85000 with Sandia      
c     Corporation, the U.S. Government retains certain rights in this
c     software.                                                      
c                                                                    
c     Redistribution and use in source and binary forms, with or     
c     without modification, are permitted provided that the following
c     conditions are met:                                            
c                                                                    
c      1. Redistributions of source code must retain the above       
c         copyright notice, this list of conditions and the following
c         disclaimer.                                                
c                                                                    
c      2. Redistributions in binary form must reproduce the above    
c         copyright notice, this list of conditions and the following
c         disclaimer in the documentation and/or other materials     
c         provided with the distribution.                            
c                                                                    
c      3. Neither the name of the Corporation nor the names of the   
c         contributors may be used to endorse or promote products    
c         derived from this software without specific prior written  
c         permission.                                                
c                                                                    
c     THIS SOFTWARE IS PROVIDED BY SANDIA CORPORATION "AS IS" AND ANY
c     EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,  
c     THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A    
c     PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL SANDIA    
c     CORPORATION OR THE CONTRIBUTORS BE LIABLE FOR ANY DIRECT,      
c     INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL     
c     DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF         
c     SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;   
c     OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF  
c     LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT      
c     (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF  
c     THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
c     SUCH DAMAGE.                                                   
c
c====================================================================
c     
c
c copyright notice
c                      Copyright 2013 Sandia Corporation             
c                                                                    
c     Under the terms of Contract DE-AC04-94AL85000 with Sandia      
c     Corporation, the U.S. Government retains certain rights in this
c     software.                                                      
c                                                                    
c     Redistribution and use in source and binary forms, with or     
c     without modification, are permitted provided that the following
c     conditions are met:                                            
c                                                                    
c      1. Redistributions of source code must retain the above       
c         copyright notice, this list of conditions and the following
c         disclaimer.                                                
c                                                                    
c      2. Redistributions in binary form must reproduce the above    
c         copyright notice, this list of conditions and the following
c         disclaimer in the documentation and/or other materials     
c         provided with the distribution.                            
c                                                                    
c      3. Neither the name of the Corporation nor the names of the   
c         contributors may be used to endorse or promote products    
c         derived from this software without specific prior written  
c         permission.                                                
c                                                                    
c     THIS SOFTWARE IS PROVIDED BY SANDIA CORPORATION "AS IS" AND ANY
c     EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,  
c     THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A    
c     PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL SANDIA    
c     CORPORATION OR THE CONTRIBUTORS BE LIABLE FOR ANY DIRECT,      
c     INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL     
c     DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF         
c     SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;   
c     OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF  
c     LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT      
c     (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF  
c     THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
c     SUCH DAMAGE.                                                   
c
c====================================================================
c     
c
c copyright notice
c                      Copyright 2013 Sandia Corporation             
c                                                                    
c     Under the terms of Contract DE-AC04-94AL85000 with Sandia      
c     Corporation, the U.S. Government retains certain rights in this
c     software.                                                      
c                                                                    
c     Redistribution and use in source and binary forms, with or     
c     without modification, are permitted provided that the following
c     conditions are met:                                            
c                                                                    
c      1. Redistributions of source code must retain the above       
c         copyright notice, this list of conditions and the following
c         disclaimer.                                                
c                                                                    
c      2. Redistributions in binary form must reproduce the above    
c         copyright notice, this list of conditions and the following
c         disclaimer in the documentation and/or other materials     
c         provided with the distribution.                            
c                                                                    
c      3. Neither the name of the Corporation nor the names of the   
c         contributors may be used to endorse or promote products    
c         derived from this software without specific prior written  
c         permission.                                                
c                                                                    
c     THIS SOFTWARE IS PROVIDED BY SANDIA CORPORATION "AS IS" AND ANY
c     EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,  
c     THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A    
c     PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL SANDIA    
c     CORPORATION OR THE CONTRIBUTORS BE LIABLE FOR ANY DIRECT,      
c     INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL     
c     DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF         
c     SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;   
c     OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF  
c     LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT      
c     (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF  
c     THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
c     SUCH DAMAGE.                                                   
c
c====================================================================
c     
c


      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT INTEGER (I-N)
c only one zone per processor, so nzne = 1
      parameter(nzne=1,ibcmax=500,jkmax=8100,nwallmax=20)
      common/original/jmax_o,kmax_o
      common/airfr/alpha,clift,cdrag,cmom
      common/bcmain_location/bcmain_directory,bcmain_filename
      common/xygrid_location/xygrid_directory,xygrid_filename
      common/dflx/dfp(jkmax,3), dfm(jkmax,3)
      common/flxs/f(jkmax,3),fv(jkmax,3)
      common/iparmi/nt,niter,ntmax,ntime,iflxo,ivis,iturb
      common/iparmr/cdis,epscon
      common/impli/impsch,njsp,nksp,nbcimp,ntjsp,ntksp
      common/implr/underr,dcoef2,dcoef4
      common/mass/pin,pout
      common/peri/kend2, kendm
      common/rmssi/jres,kres
      common/rmssr/resmax0,resmax,divmax,turres
      common/timecpu/cputime,cputiter,cpuitpt
      common/timm/beta,dtau,dt,time
      common/visc/reynum,vnu
      common/bcsurf/nbcreg, ibcval(ibcmax), nzbc(ibcmax), jkbc(ibcmax),
     & jbcb(ibcmax), jbce(ibcmax),
     & kbcb(ibcmax), kbce(ibcmax),
     & nwall, nzwall(ibcmax),
     & jkwall(ibcmax), jkinc(ibcmax),
     & jwall1(ibcmax), jwall2(ibcmax),
     & kwall1(ibcmax), kwall2(ibcmax),
     & xwallval(jkmax),ywallval(jkmax),
     & xwallv(jkmax,nwallmax),ywallv(jkmax,nwallmax)
      common/bcwake/nbcreg_wake,nzbc_t_wake(ibcmax),nzbc_b_wake(ibcmax),
     & jbcb_t_wake(ibcmax),jbce_t_wake(ibcmax),jinc_t_wake(ibcmax),
     & jbcb_b_wake(ibcmax),jbce_b_wake(ibcmax),jinc_b_wake(ibcmax)
      common/pzne/nreg, nzt(ibcmax), nzb(ibcmax),
     & jbt(ibcmax), jet(ibcmax),   ! jit(ibcmax),
     & kbt(ibcmax), ket(ibcmax),   ! kit(ibcmax),
     & jbb(ibcmax), jeb(ibcmax),   ! jib(ibcmax),
     & kbb(ibcmax), keb(ibcmax), kib(ibcmax)
      common/intpi/nintreg, nzint(ibcmax), nintrp(ibcmax),
     & iintbeg(ibcmax), iintend(ibcmax),
     & iint(ibcmax,6)
      common/unitno/istdout
      common/yaml/title,geometry
      common/gridmods/kadd
      character*60 title,geometry
      character*100 bcmain_directory,bcmain_filename
      character*100 xygrid_directory,xygrid_filename
      double precision cputime,cputiter,cpuitpt
!      
!      
!      (C) 2001 by Argonne National Laboratory.
!      See COPYRIGHT in top-level directory.
!      
!      DO NOT EDIT
!      This file created by buildiface 
!      
       INTEGER MPI_SOURCE, MPI_TAG, MPI_ERROR
       PARAMETER (MPI_SOURCE=3,MPI_TAG=4,MPI_ERROR=5)
       INTEGER MPI_STATUS_SIZE
       PARAMETER (MPI_STATUS_SIZE=5)
       INTEGER MPI_STATUS_IGNORE(MPI_STATUS_SIZE)
       INTEGER MPI_STATUSES_IGNORE(MPI_STATUS_SIZE,1)
       INTEGER MPI_ERRCODES_IGNORE(1)
       CHARACTER*1 MPI_ARGVS_NULL(1,1)
       CHARACTER*1 MPI_ARGV_NULL(1)
       INTEGER MPI_SUCCESS
       PARAMETER (MPI_SUCCESS=0)
       INTEGER MPI_ERR_KEYVAL
       PARAMETER (MPI_ERR_KEYVAL=48)
       INTEGER MPI_ERR_BUFFER
       PARAMETER (MPI_ERR_BUFFER=1)
       INTEGER MPI_ERR_RMA_FLAVOR
       PARAMETER (MPI_ERR_RMA_FLAVOR=58)
       INTEGER MPI_ERR_RMA_SYNC
       PARAMETER (MPI_ERR_RMA_SYNC=50)
       INTEGER MPI_ERR_FILE_EXISTS
       PARAMETER (MPI_ERR_FILE_EXISTS=25)
       INTEGER MPI_ERR_WIN
       PARAMETER (MPI_ERR_WIN=45)
       INTEGER MPI_ERR_UNKNOWN
       PARAMETER (MPI_ERR_UNKNOWN=13)
       INTEGER MPI_ERR_SERVICE
       PARAMETER (MPI_ERR_SERVICE=41)
       INTEGER MPI_ERR_BASE
       PARAMETER (MPI_ERR_BASE=46)
       INTEGER MPI_ERR_LOCKTYPE
       PARAMETER (MPI_ERR_LOCKTYPE=47)
       INTEGER MPI_ERR_INFO_VALUE
       PARAMETER (MPI_ERR_INFO_VALUE=30)
       INTEGER MPI_ERR_ASSERT
       PARAMETER (MPI_ERR_ASSERT=53)
       INTEGER MPI_ERR_TYPE
       PARAMETER (MPI_ERR_TYPE=3)
       INTEGER MPI_ERR_NO_SPACE
       PARAMETER (MPI_ERR_NO_SPACE=36)
       INTEGER MPI_ERR_INFO
       PARAMETER (MPI_ERR_INFO=28)
       INTEGER MPI_ERR_READ_ONLY
       PARAMETER (MPI_ERR_READ_ONLY=40)
       INTEGER MPI_ERR_FILE_IN_USE
       PARAMETER (MPI_ERR_FILE_IN_USE=26)
       INTEGER MPI_ERR_DISP
       PARAMETER (MPI_ERR_DISP=52)
       INTEGER MPI_ERR_ARG
       PARAMETER (MPI_ERR_ARG=12)
       INTEGER MPI_ERR_RMA_ATTACH
       PARAMETER (MPI_ERR_RMA_ATTACH=56)
       INTEGER MPI_ERR_FILE
       PARAMETER (MPI_ERR_FILE=27)
       INTEGER MPI_ERR_IN_STATUS
       PARAMETER (MPI_ERR_IN_STATUS=17)
       INTEGER MPI_ERR_OTHER
       PARAMETER (MPI_ERR_OTHER=15)
       INTEGER MPI_ERR_NOT_SAME
       PARAMETER (MPI_ERR_NOT_SAME=35)
       INTEGER MPI_ERR_CONVERSION
       PARAMETER (MPI_ERR_CONVERSION=23)
       INTEGER MPI_ERR_AMODE
       PARAMETER (MPI_ERR_AMODE=21)
       INTEGER MPI_ERR_QUOTA
       PARAMETER (MPI_ERR_QUOTA=39)
       INTEGER MPI_ERR_IO
       PARAMETER (MPI_ERR_IO=32)
       INTEGER MPI_ERR_LASTCODE
       PARAMETER (MPI_ERR_LASTCODE=1073741823)
       INTEGER MPI_ERR_RMA_RANGE
       PARAMETER (MPI_ERR_RMA_RANGE=55)
       INTEGER MPI_ERR_REQUEST
       PARAMETER (MPI_ERR_REQUEST=19)
       INTEGER MPI_ERR_DUP_DATAREP
       PARAMETER (MPI_ERR_DUP_DATAREP=24)
       INTEGER MPI_ERR_DIMS
       PARAMETER (MPI_ERR_DIMS=11)
       INTEGER MPI_ERR_INFO_NOKEY
       PARAMETER (MPI_ERR_INFO_NOKEY=31)
       INTEGER MPI_ERR_COUNT
       PARAMETER (MPI_ERR_COUNT=2)
       INTEGER MPI_ERR_UNSUPPORTED_DATAREP
       PARAMETER (MPI_ERR_UNSUPPORTED_DATAREP=43)
       INTEGER MPI_ERR_NO_MEM
       PARAMETER (MPI_ERR_NO_MEM=34)
       INTEGER MPI_ERR_TOPOLOGY
       PARAMETER (MPI_ERR_TOPOLOGY=10)
       INTEGER MPI_ERR_COMM
       PARAMETER (MPI_ERR_COMM=5)
       INTEGER MPI_ERR_GROUP
       PARAMETER (MPI_ERR_GROUP=8)
       INTEGER MPI_ERR_BAD_FILE
       PARAMETER (MPI_ERR_BAD_FILE=22)
       INTEGER MPI_ERR_TAG
       PARAMETER (MPI_ERR_TAG=4)
       INTEGER MPI_ERR_SPAWN
       PARAMETER (MPI_ERR_SPAWN=42)
       INTEGER MPI_ERR_ROOT
       PARAMETER (MPI_ERR_ROOT=7)
       INTEGER MPI_ERR_UNSUPPORTED_OPERATION
       PARAMETER (MPI_ERR_UNSUPPORTED_OPERATION=44)
       INTEGER MPI_ERR_ACCESS
       PARAMETER (MPI_ERR_ACCESS=20)
       INTEGER MPI_ERR_OP
       PARAMETER (MPI_ERR_OP=9)
       INTEGER MPI_ERR_RANK
       PARAMETER (MPI_ERR_RANK=6)
       INTEGER MPI_ERR_NAME
       PARAMETER (MPI_ERR_NAME=33)
       INTEGER MPI_ERR_RMA_SHARED
       PARAMETER (MPI_ERR_RMA_SHARED=57)
       INTEGER MPI_ERR_PORT
       PARAMETER (MPI_ERR_PORT=38)
       INTEGER MPI_ERR_NO_SUCH_FILE
       PARAMETER (MPI_ERR_NO_SUCH_FILE=37)
       INTEGER MPI_ERR_INTERN
       PARAMETER (MPI_ERR_INTERN=16)
       INTEGER MPI_ERR_INFO_KEY
       PARAMETER (MPI_ERR_INFO_KEY=29)
       INTEGER MPI_ERR_SIZE
       PARAMETER (MPI_ERR_SIZE=51)
       INTEGER MPI_ERR_TRUNCATE
       PARAMETER (MPI_ERR_TRUNCATE=14)
       INTEGER MPI_ERR_PENDING
       PARAMETER (MPI_ERR_PENDING=18)
       INTEGER MPI_ERR_RMA_CONFLICT
       PARAMETER (MPI_ERR_RMA_CONFLICT=49)
       INTEGER MPI_ERRORS_ARE_FATAL
       PARAMETER (MPI_ERRORS_ARE_FATAL=1409286144)
       INTEGER MPI_ERRORS_RETURN
       PARAMETER (MPI_ERRORS_RETURN=1409286145)
       INTEGER MPI_IDENT
       PARAMETER (MPI_IDENT=0)
       INTEGER MPI_CONGRUENT
       PARAMETER (MPI_CONGRUENT=1)
       INTEGER MPI_SIMILAR
       PARAMETER (MPI_SIMILAR=2)
       INTEGER MPI_UNEQUAL
       PARAMETER (MPI_UNEQUAL=3)
       INTEGER MPI_WIN_FLAVOR_CREATE
       PARAMETER (MPI_WIN_FLAVOR_CREATE=1)
       INTEGER MPI_WIN_FLAVOR_ALLOCATE
       PARAMETER (MPI_WIN_FLAVOR_ALLOCATE=2)
       INTEGER MPI_WIN_FLAVOR_DYNAMIC
       PARAMETER (MPI_WIN_FLAVOR_DYNAMIC=3)
       INTEGER MPI_WIN_FLAVOR_SHARED
       PARAMETER (MPI_WIN_FLAVOR_SHARED=4)
       INTEGER MPI_WIN_SEPARATE
       PARAMETER (MPI_WIN_SEPARATE=1)
       INTEGER MPI_WIN_UNIFIED
       PARAMETER (MPI_WIN_UNIFIED=2)
       INTEGER MPI_MAX
       PARAMETER (MPI_MAX=1476395009)
       INTEGER MPI_MIN
       PARAMETER (MPI_MIN=1476395010)
       INTEGER MPI_SUM
       PARAMETER (MPI_SUM=1476395011)
       INTEGER MPI_PROD
       PARAMETER (MPI_PROD=1476395012)
       INTEGER MPI_LAND
       PARAMETER (MPI_LAND=1476395013)
       INTEGER MPI_BAND
       PARAMETER (MPI_BAND=1476395014)
       INTEGER MPI_LOR
       PARAMETER (MPI_LOR=1476395015)
       INTEGER MPI_BOR
       PARAMETER (MPI_BOR=1476395016)
       INTEGER MPI_LXOR
       PARAMETER (MPI_LXOR=1476395017)
       INTEGER MPI_BXOR
       PARAMETER (MPI_BXOR=1476395018)
       INTEGER MPI_MINLOC
       PARAMETER (MPI_MINLOC=1476395019)
       INTEGER MPI_MAXLOC
       PARAMETER (MPI_MAXLOC=1476395020)
       INTEGER MPI_REPLACE
       PARAMETER (MPI_REPLACE=1476395021)
       INTEGER MPI_NO_OP
       PARAMETER (MPI_NO_OP=1476395022)
       INTEGER MPI_COMM_WORLD
       PARAMETER (MPI_COMM_WORLD=1140850688)
       INTEGER MPI_COMM_SELF
       PARAMETER (MPI_COMM_SELF=1140850689)
       INTEGER MPI_GROUP_EMPTY
       PARAMETER (MPI_GROUP_EMPTY=1207959552)
       INTEGER MPI_COMM_NULL
       PARAMETER (MPI_COMM_NULL=67108864)
       INTEGER MPI_WIN_NULL
       PARAMETER (MPI_WIN_NULL=536870912)
       INTEGER MPI_FILE_NULL
       PARAMETER (MPI_FILE_NULL=0)
       INTEGER MPI_GROUP_NULL
       PARAMETER (MPI_GROUP_NULL=134217728)
       INTEGER MPI_OP_NULL
       PARAMETER (MPI_OP_NULL=402653184)
       INTEGER MPI_DATATYPE_NULL
       PARAMETER (MPI_DATATYPE_NULL=201326592)
       INTEGER MPI_REQUEST_NULL
       PARAMETER (MPI_REQUEST_NULL=738197504)
       INTEGER MPI_ERRHANDLER_NULL
       PARAMETER (MPI_ERRHANDLER_NULL=335544320)
       INTEGER MPI_INFO_NULL
       PARAMETER (MPI_INFO_NULL=469762048)
       INTEGER MPI_INFO_ENV
       PARAMETER (MPI_INFO_ENV=1543503873)
       INTEGER MPI_TAG_UB
       PARAMETER (MPI_TAG_UB=1681915906)
       INTEGER MPI_HOST
       PARAMETER (MPI_HOST=1681915908)
       INTEGER MPI_IO
       PARAMETER (MPI_IO=1681915910)
       INTEGER MPI_WTIME_IS_GLOBAL
       PARAMETER (MPI_WTIME_IS_GLOBAL=1681915912)
       INTEGER MPI_UNIVERSE_SIZE
       PARAMETER (MPI_UNIVERSE_SIZE=1681915914)
       INTEGER MPI_LASTUSEDCODE
       PARAMETER (MPI_LASTUSEDCODE=1681915916)
       INTEGER MPI_APPNUM
       PARAMETER (MPI_APPNUM=1681915918)
       INTEGER MPI_WIN_BASE
       PARAMETER (MPI_WIN_BASE=1711276034)
       INTEGER MPI_WIN_SIZE
       PARAMETER (MPI_WIN_SIZE=1711276036)
       INTEGER MPI_WIN_DISP_UNIT
       PARAMETER (MPI_WIN_DISP_UNIT=1711276038)
       INTEGER MPI_WIN_CREATE_FLAVOR
       PARAMETER (MPI_WIN_CREATE_FLAVOR=1711276040)
       INTEGER MPI_WIN_MODEL
       PARAMETER (MPI_WIN_MODEL=1711276042)
       INTEGER MPI_MAX_ERROR_STRING
       PARAMETER (MPI_MAX_ERROR_STRING=512-1)
       INTEGER MPI_MAX_PORT_NAME
       PARAMETER (MPI_MAX_PORT_NAME=255)
       INTEGER MPI_MAX_OBJECT_NAME
       PARAMETER (MPI_MAX_OBJECT_NAME=127)
       INTEGER MPI_MAX_INFO_KEY
       PARAMETER (MPI_MAX_INFO_KEY=254)
       INTEGER MPI_MAX_INFO_VAL
       PARAMETER (MPI_MAX_INFO_VAL=1023)
       INTEGER MPI_MAX_PROCESSOR_NAME
       PARAMETER (MPI_MAX_PROCESSOR_NAME=128-1)
       INTEGER MPI_MAX_DATAREP_STRING
       PARAMETER (MPI_MAX_DATAREP_STRING=127)
       INTEGER MPI_MAX_LIBRARY_VERSION_STRING
       PARAMETER (MPI_MAX_LIBRARY_VERSION_STRING=8192-1)
       INTEGER MPI_UNDEFINED
       PARAMETER (MPI_UNDEFINED=(-32766))
       INTEGER MPI_KEYVAL_INVALID
       PARAMETER (MPI_KEYVAL_INVALID=603979776)
       INTEGER MPI_BSEND_OVERHEAD
       PARAMETER (MPI_BSEND_OVERHEAD=96)
       INTEGER MPI_PROC_NULL
       PARAMETER (MPI_PROC_NULL=-1)
       INTEGER MPI_ANY_SOURCE
       PARAMETER (MPI_ANY_SOURCE=-2)
       INTEGER MPI_ANY_TAG
       PARAMETER (MPI_ANY_TAG=-1)
       INTEGER MPI_ROOT
       PARAMETER (MPI_ROOT=-3)
       INTEGER MPI_GRAPH
       PARAMETER (MPI_GRAPH=1)
       INTEGER MPI_CART
       PARAMETER (MPI_CART=2)
       INTEGER MPI_DIST_GRAPH
       PARAMETER (MPI_DIST_GRAPH=3)
       INTEGER MPI_VERSION
       PARAMETER (MPI_VERSION=3)
       INTEGER MPI_SUBVERSION
       PARAMETER (MPI_SUBVERSION=1)
       INTEGER MPI_LOCK_EXCLUSIVE
       PARAMETER (MPI_LOCK_EXCLUSIVE=234)
       INTEGER MPI_LOCK_SHARED
       PARAMETER (MPI_LOCK_SHARED=235)
       INTEGER MPI_COMPLEX
       PARAMETER (MPI_COMPLEX=1275070494)
       INTEGER MPI_DOUBLE_COMPLEX
       PARAMETER (MPI_DOUBLE_COMPLEX=1275072546)
       INTEGER MPI_LOGICAL
       PARAMETER (MPI_LOGICAL=1275069469)
       INTEGER MPI_REAL
       PARAMETER (MPI_REAL=1275069468)
       INTEGER MPI_DOUBLE_PRECISION
       PARAMETER (MPI_DOUBLE_PRECISION=1275070495)
       INTEGER MPI_INTEGER
       PARAMETER (MPI_INTEGER=1275069467)
       INTEGER MPI_2INTEGER
       PARAMETER (MPI_2INTEGER=1275070496)
       INTEGER MPI_2DOUBLE_PRECISION
       PARAMETER (MPI_2DOUBLE_PRECISION=1275072547)
       INTEGER MPI_2REAL
       PARAMETER (MPI_2REAL=1275070497)
       INTEGER MPI_CHARACTER
       PARAMETER (MPI_CHARACTER=1275068698)
       INTEGER MPI_BYTE
       PARAMETER (MPI_BYTE=1275068685)
       INTEGER MPI_UB
       PARAMETER (MPI_UB=1275068433)
       INTEGER MPI_LB
       PARAMETER (MPI_LB=1275068432)
       INTEGER MPI_PACKED
       PARAMETER (MPI_PACKED=1275068687)
       INTEGER MPI_INTEGER1
       PARAMETER (MPI_INTEGER1=1275068717)
       INTEGER MPI_INTEGER2
       PARAMETER (MPI_INTEGER2=1275068975)
       INTEGER MPI_INTEGER4
       PARAMETER (MPI_INTEGER4=1275069488)
       INTEGER MPI_INTEGER8
       PARAMETER (MPI_INTEGER8=1275070513)
       INTEGER MPI_INTEGER16
       PARAMETER (MPI_INTEGER16=MPI_DATATYPE_NULL)
       INTEGER MPI_REAL4
       PARAMETER (MPI_REAL4=1275069479)
       INTEGER MPI_REAL8
       PARAMETER (MPI_REAL8=1275070505)
       INTEGER MPI_REAL16
       PARAMETER (MPI_REAL16=1275072555)
       INTEGER MPI_COMPLEX8
       PARAMETER (MPI_COMPLEX8=1275070504)
       INTEGER MPI_COMPLEX16
       PARAMETER (MPI_COMPLEX16=1275072554)
       INTEGER MPI_COMPLEX32
       PARAMETER (MPI_COMPLEX32=1275076652)
       INTEGER MPI_ADDRESS_KIND
       PARAMETER (MPI_ADDRESS_KIND=8)
       INTEGER MPI_OFFSET_KIND
       PARAMETER (MPI_OFFSET_KIND=8)
       INTEGER MPI_COUNT_KIND
       PARAMETER (MPI_COUNT_KIND=8)
       INTEGER MPI_INTEGER_KIND
       PARAMETER (MPI_INTEGER_KIND=4)
       INTEGER MPI_CHAR
       PARAMETER (MPI_CHAR=1275068673)
       INTEGER MPI_SIGNED_CHAR
       PARAMETER (MPI_SIGNED_CHAR=1275068696)
       INTEGER MPI_UNSIGNED_CHAR
       PARAMETER (MPI_UNSIGNED_CHAR=1275068674)
       INTEGER MPI_WCHAR
       PARAMETER (MPI_WCHAR=1275069454)
       INTEGER MPI_SHORT
       PARAMETER (MPI_SHORT=1275068931)
       INTEGER MPI_UNSIGNED_SHORT
       PARAMETER (MPI_UNSIGNED_SHORT=1275068932)
       INTEGER MPI_INT
       PARAMETER (MPI_INT=1275069445)
       INTEGER MPI_UNSIGNED
       PARAMETER (MPI_UNSIGNED=1275069446)
       INTEGER MPI_LONG
       PARAMETER (MPI_LONG=1275070471)
       INTEGER MPI_UNSIGNED_LONG
       PARAMETER (MPI_UNSIGNED_LONG=1275070472)
       INTEGER MPI_FLOAT
       PARAMETER (MPI_FLOAT=1275069450)
       INTEGER MPI_DOUBLE
       PARAMETER (MPI_DOUBLE=1275070475)
       INTEGER MPI_LONG_DOUBLE
       PARAMETER (MPI_LONG_DOUBLE=1275072524)
       INTEGER MPI_LONG_LONG_INT
       PARAMETER (MPI_LONG_LONG_INT=1275070473)
       INTEGER MPI_UNSIGNED_LONG_LONG
       PARAMETER (MPI_UNSIGNED_LONG_LONG=1275070489)
       INTEGER MPI_LONG_LONG
       PARAMETER (MPI_LONG_LONG=1275070473)
       INTEGER MPI_FLOAT_INT
       PARAMETER (MPI_FLOAT_INT=-1946157056)
       INTEGER MPI_DOUBLE_INT
       PARAMETER (MPI_DOUBLE_INT=-1946157055)
       INTEGER MPI_LONG_INT
       PARAMETER (MPI_LONG_INT=-1946157054)
       INTEGER MPI_SHORT_INT
       PARAMETER (MPI_SHORT_INT=-1946157053)
       INTEGER MPI_2INT
       PARAMETER (MPI_2INT=1275070486)
       INTEGER MPI_LONG_DOUBLE_INT
       PARAMETER (MPI_LONG_DOUBLE_INT=-1946157052)
       INTEGER MPI_INT8_T
       PARAMETER (MPI_INT8_T=1275068727)
       INTEGER MPI_INT16_T
       PARAMETER (MPI_INT16_T=1275068984)
       INTEGER MPI_INT32_T
       PARAMETER (MPI_INT32_T=1275069497)
       INTEGER MPI_INT64_T
       PARAMETER (MPI_INT64_T=1275070522)
       INTEGER MPI_UINT8_T
       PARAMETER (MPI_UINT8_T=1275068731)
       INTEGER MPI_UINT16_T
       PARAMETER (MPI_UINT16_T=1275068988)
       INTEGER MPI_UINT32_T
       PARAMETER (MPI_UINT32_T=1275069501)
       INTEGER MPI_UINT64_T
       PARAMETER (MPI_UINT64_T=1275070526)
       INTEGER MPI_C_BOOL
       PARAMETER (MPI_C_BOOL=1275068735)
       INTEGER MPI_C_FLOAT_COMPLEX
       PARAMETER (MPI_C_FLOAT_COMPLEX=1275070528)
       INTEGER MPI_C_COMPLEX
       PARAMETER (MPI_C_COMPLEX=1275070528)
       INTEGER MPI_C_DOUBLE_COMPLEX
       PARAMETER (MPI_C_DOUBLE_COMPLEX=1275072577)
       INTEGER MPI_C_LONG_DOUBLE_COMPLEX
       PARAMETER (MPI_C_LONG_DOUBLE_COMPLEX=1275076674)
       INTEGER MPI_AINT
       PARAMETER (MPI_AINT=1275070531)
       INTEGER MPI_OFFSET
       PARAMETER (MPI_OFFSET=1275070532)
       INTEGER MPI_COUNT
       PARAMETER (MPI_COUNT=1275070533)
       INTEGER MPI_CXX_BOOL
       PARAMETER (MPI_CXX_BOOL=1275068723)
       INTEGER MPI_CXX_FLOAT_COMPLEX
       PARAMETER (MPI_CXX_FLOAT_COMPLEX=1275070516)
       INTEGER MPI_CXX_DOUBLE_COMPLEX
       PARAMETER (MPI_CXX_DOUBLE_COMPLEX=1275072565)
       INTEGER MPI_CXX_LONG_DOUBLE_COMPLEX
       PARAMETER (MPI_CXX_LONG_DOUBLE_COMPLEX=1275076662)
       INTEGER MPI_COMBINER_NAMED
       PARAMETER (MPI_COMBINER_NAMED=1)
       INTEGER MPI_COMBINER_DUP
       PARAMETER (MPI_COMBINER_DUP=2)
       INTEGER MPI_COMBINER_CONTIGUOUS
       PARAMETER (MPI_COMBINER_CONTIGUOUS=3)
       INTEGER MPI_COMBINER_VECTOR
       PARAMETER (MPI_COMBINER_VECTOR=4)
       INTEGER MPI_COMBINER_HVECTOR_INTEGER
       PARAMETER (MPI_COMBINER_HVECTOR_INTEGER=5)
       INTEGER MPI_COMBINER_HVECTOR
       PARAMETER (MPI_COMBINER_HVECTOR=6)
       INTEGER MPI_COMBINER_INDEXED
       PARAMETER (MPI_COMBINER_INDEXED=7)
       INTEGER MPI_COMBINER_HINDEXED_INTEGER
       PARAMETER (MPI_COMBINER_HINDEXED_INTEGER=8)
       INTEGER MPI_COMBINER_HINDEXED
       PARAMETER (MPI_COMBINER_HINDEXED=9)
       INTEGER MPI_COMBINER_INDEXED_BLOCK
       PARAMETER (MPI_COMBINER_INDEXED_BLOCK=10)
       INTEGER MPI_COMBINER_STRUCT_INTEGER
       PARAMETER (MPI_COMBINER_STRUCT_INTEGER=11)
       INTEGER MPI_COMBINER_STRUCT
       PARAMETER (MPI_COMBINER_STRUCT=12)
       INTEGER MPI_COMBINER_SUBARRAY
       PARAMETER (MPI_COMBINER_SUBARRAY=13)
       INTEGER MPI_COMBINER_DARRAY
       PARAMETER (MPI_COMBINER_DARRAY=14)
       INTEGER MPI_COMBINER_F90_REAL
       PARAMETER (MPI_COMBINER_F90_REAL=15)
       INTEGER MPI_COMBINER_F90_COMPLEX
       PARAMETER (MPI_COMBINER_F90_COMPLEX=16)
       INTEGER MPI_COMBINER_F90_INTEGER
       PARAMETER (MPI_COMBINER_F90_INTEGER=17)
       INTEGER MPI_COMBINER_RESIZED
       PARAMETER (MPI_COMBINER_RESIZED=18)
       INTEGER MPI_COMBINER_HINDEXED_BLOCK
       PARAMETER (MPI_COMBINER_HINDEXED_BLOCK=19)
       INTEGER MPI_TYPECLASS_REAL
       PARAMETER (MPI_TYPECLASS_REAL=1)
       INTEGER MPI_TYPECLASS_INTEGER
       PARAMETER (MPI_TYPECLASS_INTEGER=2)
       INTEGER MPI_TYPECLASS_COMPLEX
       PARAMETER (MPI_TYPECLASS_COMPLEX=3)
       INTEGER MPI_MODE_NOCHECK
       PARAMETER (MPI_MODE_NOCHECK=1024)
       INTEGER MPI_MODE_NOSTORE
       PARAMETER (MPI_MODE_NOSTORE=2048)
       INTEGER MPI_MODE_NOPUT
       PARAMETER (MPI_MODE_NOPUT=4096)
       INTEGER MPI_MODE_NOPRECEDE
       PARAMETER (MPI_MODE_NOPRECEDE=8192)
       INTEGER MPI_MODE_NOSUCCEED
       PARAMETER (MPI_MODE_NOSUCCEED=16384)
       INTEGER MPI_COMM_TYPE_SHARED
       PARAMETER (MPI_COMM_TYPE_SHARED=1)
       INTEGER MPI_MESSAGE_NULL
       PARAMETER (MPI_MESSAGE_NULL=738197504)
       INTEGER MPI_MESSAGE_NO_PROC
       PARAMETER (MPI_MESSAGE_NO_PROC=1811939328)
       INTEGER MPI_THREAD_SINGLE
       PARAMETER (MPI_THREAD_SINGLE=0)
       INTEGER MPI_THREAD_FUNNELED
       PARAMETER (MPI_THREAD_FUNNELED=1)
       INTEGER MPI_THREAD_SERIALIZED
       PARAMETER (MPI_THREAD_SERIALIZED=2)
       INTEGER MPI_THREAD_MULTIPLE
       PARAMETER (MPI_THREAD_MULTIPLE=3)
       INTEGER MPI_MODE_RDONLY
       PARAMETER (MPI_MODE_RDONLY=2)
       INTEGER MPI_MODE_RDWR
       PARAMETER (MPI_MODE_RDWR=8)
       INTEGER MPI_MODE_WRONLY
       PARAMETER (MPI_MODE_WRONLY=4)
       INTEGER MPI_MODE_DELETE_ON_CLOSE
       PARAMETER (MPI_MODE_DELETE_ON_CLOSE=16)
       INTEGER MPI_MODE_UNIQUE_OPEN
       PARAMETER (MPI_MODE_UNIQUE_OPEN=32)
       INTEGER MPI_MODE_CREATE
       PARAMETER (MPI_MODE_CREATE=1)
       INTEGER MPI_MODE_EXCL
       PARAMETER (MPI_MODE_EXCL=64)
       INTEGER MPI_MODE_APPEND
       PARAMETER (MPI_MODE_APPEND=128)
       INTEGER MPI_MODE_SEQUENTIAL
       PARAMETER (MPI_MODE_SEQUENTIAL=256)
       INTEGER MPI_SEEK_SET
       PARAMETER (MPI_SEEK_SET=600)
       INTEGER MPI_SEEK_CUR
       PARAMETER (MPI_SEEK_CUR=602)
       INTEGER MPI_SEEK_END
       PARAMETER (MPI_SEEK_END=604)
       INTEGER MPI_ORDER_C
       PARAMETER (MPI_ORDER_C=56)
       INTEGER MPI_ORDER_FORTRAN
       PARAMETER (MPI_ORDER_FORTRAN=57)
       INTEGER MPI_DISTRIBUTE_BLOCK
       PARAMETER (MPI_DISTRIBUTE_BLOCK=121)
       INTEGER MPI_DISTRIBUTE_CYCLIC
       PARAMETER (MPI_DISTRIBUTE_CYCLIC=122)
       INTEGER MPI_DISTRIBUTE_NONE
       PARAMETER (MPI_DISTRIBUTE_NONE=123)
       INTEGER MPI_DISTRIBUTE_DFLT_DARG
       PARAMETER (MPI_DISTRIBUTE_DFLT_DARG=-49767)
       integer*8 MPI_DISPLACEMENT_CURRENT
       PARAMETER (MPI_DISPLACEMENT_CURRENT=-54278278)
       LOGICAL MPI_SUBARRAYS_SUPPORTED
       PARAMETER(MPI_SUBARRAYS_SUPPORTED=.FALSE.)
       LOGICAL MPI_ASYNC_PROTECTS_NONBLOCKING
       PARAMETER(MPI_ASYNC_PROTECTS_NONBLOCKING=.FALSE.)
       INTEGER MPI_BOTTOM, MPI_IN_PLACE, MPI_UNWEIGHTED
       INTEGER MPI_WEIGHTS_EMPTY
       EXTERNAL MPI_DUP_FN, MPI_NULL_DELETE_FN, MPI_NULL_COPY_FN
       EXTERNAL MPI_WTIME, MPI_WTICK
       EXTERNAL PMPI_WTIME, PMPI_WTICK
       EXTERNAL MPI_COMM_DUP_FN, MPI_COMM_NULL_DELETE_FN
       EXTERNAL MPI_COMM_NULL_COPY_FN
       EXTERNAL MPI_WIN_DUP_FN, MPI_WIN_NULL_DELETE_FN
       EXTERNAL MPI_WIN_NULL_COPY_FN
       EXTERNAL MPI_TYPE_DUP_FN, MPI_TYPE_NULL_DELETE_FN
       EXTERNAL MPI_TYPE_NULL_COPY_FN
       EXTERNAL MPI_CONVERSION_FN_NULL
       REAL*8 MPI_WTIME, MPI_WTICK
       REAL*8 PMPI_WTIME, PMPI_WTICK


       COMMON /MPIFCMB5/ MPI_UNWEIGHTED
       COMMON /MPIFCMB9/ MPI_WEIGHTS_EMPTY
       SAVE /MPIFCMB5/
       SAVE /MPIFCMB9/

       COMMON /MPIPRIV1/ MPI_BOTTOM, MPI_IN_PLACE, MPI_STATUS_IGNORE

       COMMON /MPIPRIV2/ MPI_STATUSES_IGNORE, MPI_ERRCODES_IGNORE
       SAVE /MPIPRIV1/,/MPIPRIV2/

       COMMON /MPIPRIVC/ MPI_ARGVS_NULL, MPI_ARGV_NULL
       SAVE   /MPIPRIVC/
c copyright notice
c                      Copyright 2013 Sandia Corporation             
c                                                                    
c     Under the terms of Contract DE-AC04-94AL85000 with Sandia      
c     Corporation, the U.S. Government retains certain rights in this
c     software.                                                      
c                                                                    
c     Redistribution and use in source and binary forms, with or     
c     without modification, are permitted provided that the following
c     conditions are met:                                            
c                                                                    
c      1. Redistributions of source code must retain the above       
c         copyright notice, this list of conditions and the following
c         disclaimer.                                                
c                                                                    
c      2. Redistributions in binary form must reproduce the above    
c         copyright notice, this list of conditions and the following
c         disclaimer in the documentation and/or other materials     
c         provided with the distribution.                            
c                                                                    
c      3. Neither the name of the Corporation nor the names of the   
c         contributors may be used to endorse or promote products    
c         derived from this software without specific prior written  
c         permission.                                                
c                                                                    
c     THIS SOFTWARE IS PROVIDED BY SANDIA CORPORATION "AS IS" AND ANY
c     EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,  
c     THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A    
c     PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL SANDIA    
c     CORPORATION OR THE CONTRIBUTORS BE LIABLE FOR ANY DIRECT,      
c     INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL     
c     DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF         
c     SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;   
c     OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF  
c     LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT      
c     (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF  
c     THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
c     SUCH DAMAGE.                                                   
c
c====================================================================
c     
c
      common/mpi_params/nodeid,numprocs
      integer nodeid,numprocs,ierr,stat(MPI_STATUS_SIZE)

      logical kpr
      logical DEBUG

      DEBUG=.false.
c      DEBUG = .true.

c-----
c DEFAULT SETTINGS 
c-----
      kpr = .false.
      njswp = 2
      nkswp = 2
      ntjsp = 2
      ntksp = 0
      iflxodr = 3
      itran = 0
      xtr1 = 0.0
      xtr2 = 0.0
      cdiss = 1.0
   
      njsp = njswp
      nksp = nkswp
      kend2 = 2
      kendm = kmax - 1
      iflxo = iflxodr
      itrans = itran
      cdis = cdiss
      xtran1 = xtr1
      xtran2 = xtr2

      if (nodeid.eq.0) then
       print*
       print*, ' Settings from initia.f/inizone:'
       print*, '  kpr = ',kpr
       print*, '  njswp = ',njswp
       print*, '  nkswp = ',nkswp
       print*, '  ntjsp = ',ntjsp
       print*, '  ntksp = ',ntksp
       print*, '  iflxodr = ',iflxodr
       print*, '  itran = ',itran
       print*, '  xtr1 = ',xtr1
       print*, '  xtr2 = ',xtr2
       print*, '  cdiss = ',cdiss
       print*, '  njsp = ',njsp
       print*, '  nksp = ',nksp
       print*, '  kend2 = ',kend2
       print*, '  kendm = ',kendm
       print*, '  iflxo = ',iflxo
       print*, '  itrans = ',itrans
       print*, '  cdis = ',cdis
       print*, '  xtran1 = ',xtran1
       print*, '  xtran2 = ',xtran2
       print*, '    ---  end ---'
       print*
      endif


      if (DEBUG) then
         print*
         print*, ' >> Node ',nodeid,': finished in initia.f/inizone '
      endif

c-----
c  End inizone
c-----
      return
      end
c
c
