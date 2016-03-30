#
#  -- LAPACK95 SRC makefile (version 2.0) --
#     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
#     August, 2000
#
include ../make.inc

SCDZOBJS = la_auxmod.o $(SCDZOBMOD) $(OBJAU) $(SOBJS) $(COBJS) $(DOBJS) $(ZOBJS)

SCDZOBMOD  = f77_lapack_single_double_complex_dcomplex.o \
	f95_lapack_single_double_complex_dcomplex.o

OBJAU  = la_erinfo.o la_ws_gels.o la_ws_gelss.o

SOBJS = la_sgesv.o la_sgesv1.o la_sgesvx.o la_sgesvx1.o \
	la_sgetrf.o la_slagge.o la_slange.o la_slange1.o \
	la_sgetri.o la_sgetrs.o la_sgetrs1.o la_sgeequ.o \
	la_sgerfs.o la_sgerfs1.o la_ssytrd.o la_ssygst.o \
	la_sorgtr.o la_spotrf.o la_sgbtrf.o la_slamch.o \
	la_sgbsv.o la_sgbsv1.o la_sgbsvx.o la_sgbsvx1.o \
	la_sgtsv.o la_sgtsv1.o la_sgtsvx.o la_sgtsvx1.o \
	la_sposv.o la_sposv1.o la_sposvx.o la_sposvx1.o \
	la_sppsv.o la_sppsv1.o la_sppsvx.o la_sppsvx1.o \
	la_spbsv.o la_spbsv1.o la_spbsvx.o la_spbsvx1.o \
	la_sptsv.o la_sptsv1.o la_sptsvx.o la_sptsvx1.o \
	la_ssysv.o la_ssysv1.o la_ssysvx.o la_ssysvx1.o \
	la_sspsv.o la_sspsv1.o la_sspsvx.o la_sspsvx1.o \
	la_sgels.o la_sgels1.o la_sgelsx.o la_sgelsx1.o \
	la_sgelss.o la_sgelss1.o la_sgglse.o la_sggglm.o \
	la_ssyev.o la_ssyevd.o la_ssyevx.o la_sspev.o \
	la_sspevd.o la_sspevx.o la_ssbev.o la_ssbevd.o \
	la_ssbevx.o la_sstev.o la_sstevd.o la_sstevx.o \
	la_sgees.o la_sgeesx.o la_sgeev.o la_sgeevx.o \
	la_sgesvd.o la_ssygv.o la_sspgv.o la_ssbgv.o \
	la_sgegs.o la_sgegv.o la_sggsvd.o la_sgelsy.o \
	la_sgelsy1.o la_sspgvd.o la_ssbgvd.o la_ssygvd.o \
	la_ssbgvx.o la_sspgvx.o la_sggev.o la_sgges.o \
	la_sstevr.o la_sgelsd.o la_sgelsd1.o la_ssyevr.o \
	la_sggevx.o la_ssygvx.o la_sggesx.o la_sgesdd.o

DOBJS = la_dgesv.o la_dgesv1.o la_dgesvx.o la_dgesvx1.o \
	la_dgetrf.o la_dlagge.o la_dlange.o la_dlange1.o \
	la_dgetri.o la_dgetrs.o la_dgetrs1.o la_dgeequ.o \
	la_dgerfs.o la_dgerfs1.o la_dsytrd.o la_dsygst.o \
	la_dorgtr.o la_dpotrf.o la_dgbtrf.o la_dlamch.o \
	la_dgbsv.o la_dgbsv1.o la_dgbsvx.o la_dgbsvx1.o \
	la_dgtsv.o la_dgtsv1.o la_dgtsvx.o la_dgtsvx1.o \
	la_dposv.o la_dposv1.o la_dposvx.o la_dposvx1.o \
	la_dppsv.o la_dppsv1.o la_dppsvx.o la_dppsvx1.o \
	la_dpbsv.o la_dpbsv1.o la_dpbsvx.o la_dpbsvx1.o \
	la_dptsv.o la_dptsv1.o la_dptsvx.o la_dptsvx1.o \
	la_dsysv.o la_dsysv1.o la_dsysvx.o la_dsysvx1.o \
	la_dspsv.o la_dspsv1.o la_dspsvx.o la_dspsvx1.o \
	la_dgels.o la_dgels1.o la_dgelsx.o la_dgelsx1.o \
	la_dgelss.o la_dgelss1.o la_dgglse.o la_dggglm.o \
	la_dsyev.o la_dsyevd.o la_dsyevx.o la_dspev.o \
	la_dspevd.o la_dspevx.o la_dsbev.o la_dsbevd.o \
	la_dsbevx.o la_dstev.o la_dstevd.o la_dstevx.o \
	la_dgees.o la_dgeesx.o la_dgeev.o la_dgeevx.o \
	la_dgesvd.o la_dsygv.o la_dspgv.o la_dsbgv.o \
	la_dgegs.o la_dgegv.o la_dggsvd.o la_dgelsy.o \
	la_dgelsy1.o la_dspgvd.o la_dsbgvd.o la_dsygvd.o \
	la_dsbgvx.o la_dspgvx.o la_dggev.o la_dgges.o \
	la_dstevr.o la_dgelsd.o la_dgelsd1.o la_dsyevr.o \
	la_dggevx.o la_dsygvx.o la_dggesx.o la_dgesdd.o

COBJS = la_cgesv.o la_cgesv1.o la_cgesvx.o la_cgesvx1.o \
	la_cgetrf.o la_clagge.o la_clange.o la_clange1.o \
	la_cgetri.o la_cgetrs.o la_cgetrs1.o la_cgeequ.o \
	la_cgerfs.o la_cgerfs1.o la_chetrd.o la_chegst.o \
	la_cungtr.o la_cpotrf.o la_cgbtrf.o \
	la_cgbsv.o la_cgbsv1.o la_cgbsvx.o la_cgbsvx1.o \
	la_cgtsv.o la_cgtsv1.o la_cgtsvx.o la_cgtsvx1.o \
	la_cposv.o la_cposv1.o la_cposvx.o la_cposvx1.o \
	la_cppsv.o la_cppsv1.o la_cppsvx.o la_cppsvx1.o \
	la_cpbsv.o la_cpbsv1.o la_cpbsvx.o la_cpbsvx1.o \
	la_cptsv.o la_cptsv1.o la_cptsvx.o la_cptsvx1.o \
	la_csysv.o la_csysv1.o la_csysvx.o la_csysvx1.o \
	la_cspsv.o la_cspsv1.o la_cspsvx.o la_cspsvx1.o \
	la_chesv.o la_chesv1.o la_chesvx.o la_chesvx1.o \
	la_chpsv.o la_chpsv1.o la_chpsvx.o la_chpsvx1.o \
	la_cgels.o la_cgels1.o la_cgelsx.o la_cgelsx1.o \
	la_cgelss.o la_cgelss1.o la_cgglse.o la_cggglm.o\
	la_cheev.o la_cheevd.o la_cheevx.o la_chpev.o \
	la_chpevd.o la_chpevx.o la_chbev.o la_chbevd.o \
	la_chbevx.o la_chbgvx.o la_chpgvx.o \
	la_cgees.o la_cgeesx.o la_cgeev.o la_cgeevx.o \
	la_cgesvd.o la_chegv.o la_chpgv.o la_chbgv.o \
	la_cgegs.o la_cgegv.o la_cggsvd.o la_cgelsy.o \
	la_cgelsy1.o la_chpgvd.o la_chbgvd.o la_chegvd.o \
	la_cggev.o la_cgges.o la_cgelsd.o la_cgelsd1.o \
	la_cheevr.o la_cggevx.o la_chegvx.o la_cggesx.o \
	la_cgesdd.o

ZOBJS = la_zgesv.o la_zgesv1.o la_zgesvx.o la_zgesvx1.o \
	la_zgetrf.o la_zlagge.o la_zlange.o la_zlange1.o \
	la_zgetri.o la_zgetrs.o la_zgetrs1.o la_zgeequ.o \
	la_zgerfs.o la_zgerfs1.o la_zhetrd.o la_zhegst.o \
	la_zungtr.o la_zpotrf.o la_zgbtrf.o \
	la_zgbsv.o la_zgbsv1.o la_zgbsvx.o la_zgbsvx1.o \
	la_zgtsv.o la_zgtsv1.o la_zgtsvx.o la_zgtsvx1.o \
	la_zposv.o la_zposv1.o la_zposvx.o la_zposvx1.o \
	la_zppsv.o la_zppsv1.o la_zppsvx.o la_zppsvx1.o \
	la_zpbsv.o la_zpbsv1.o la_zpbsvx.o la_zpbsvx1.o \
	la_zptsv.o la_zptsv1.o la_zptsvx.o la_zptsvx1.o \
	la_zsysv.o la_zsysv1.o la_zsysvx.o la_zsysvx1.o \
	la_zspsv.o la_zspsv1.o la_zspsvx.o la_zspsvx1.o \
	la_zhesv.o la_zhesv1.o la_zhesvx.o la_zhesvx1.o \
	la_zhpsv.o la_zhpsv1.o la_zhpsvx.o la_zhpsvx1.o \
	la_zgels.o la_zgels1.o la_zgelsx.o la_zgelsx1.o \
	la_zgelss.o la_zgelss1.o la_zgglse.o la_zggglm.o \
	la_zheev.o la_zheevd.o la_zheevx.o la_zhpev.o \
	la_zhpevd.o la_zhpevx.o la_zhbev.o la_zhbevd.o \
	la_zhbevx.o la_zhbgvx.o la_zhpgvx.o \
	la_zgees.o la_zgeesx.o la_zgeev.o la_zgeevx.o \
	la_zgesvd.o la_zhegv.o la_zhpgv.o la_zhbgv.o \
	la_zgegs.o la_zgegv.o la_zggsvd.o la_zgelsy.o \
	la_zgelsy1.o la_zhpgvd.o la_zhbgvd.o la_zhegvd.o \
	la_zggev.o la_zgges.o la_zgelsd.o la_zgelsd1.o \
	la_zheevr.o la_zggevx.o la_zhegvx.o la_zggesx.o \
	la_zgesdd.o

aux: la_auxmod.o

single_double_complex_dcomplex: $(SCDZOBJS)
	rm -f ../lapack95.a
	ar cr ../lapack95.a $(SCDZOBJS)
	ranlib ../lapack95.a
	'cp' *.mod ../
	rm -f f77_lapack.* f95_lapack.*
	rm -f *_lapack_single_double_complex_dcomplex.o

clean: 
	'rm' -f *.a *.o *.mod *.l core
