!>@file   check_sph_monitor_header.f90
!!@brief  module check_sph_monitor_header
!!
!!@author H. Matsui
!!@date Programmed in 2009
!
!>@brief  I/O routines for mean square and averaga data
!!
!!@verbatim
!!      logical function error_sph_vol_monitor_head(id_file, mode_label,&
!!     &                nri_sph, ltr_sph, nlayer_ICB, nlayer_CMB,       &
!!     &                kr_inside, r_inside, kr_outside, r_outside,     &
!!     &                num_fld_sq, num_comp_sq, pwr_name,              &
!!     &                ntot_comp_sq, pwr_label)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len = kchara), intent(in) :: mode_label
!!        integer(kind = kint), intent(in) :: nri_sph, ltr_sph
!!        integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
!!        integer(kind = kint), intent(in) :: kr_inside, kr_outside
!!        real(kind = kreal), intent(in) ::   r_inside,  r_outside
!!        integer(kind = kint), intent(in) :: num_fld_sq, ntot_comp_sq
!!        integer(kind = kint), intent(in) :: num_comp_sq(num_fld_sq)
!!        character(len=kchara), intent(in) :: pwr_name(num_fld_sq)
!!        character (len=kchara), intent(in) :: pwr_label(ntot_comp_sq)
!!      logical function error_sph_moniter_two_int(id_file, label,      &
!!     &                                           int_ref1, int_ref2)
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len=kchara), intent(in) :: label(2)
!!        integer(kind = kint), intent(in) :: int_ref1, int_ref2
!!      logical function error_sph_moniter_int_real(id_file, label,     &
!!     &                                            int_ref, real_ref)
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len=kchara), intent(in) :: label(2)
!!        integer(kind = kint), intent(in) :: int_ref
!!        real(kind = kreal), intent(in) :: real_ref
!!      logical function error_sph_moniter_ncomp(id_file, num_fld_sq,   &
!!     &                                         num_comp_sq, pwr_name)
!!        integer(kind = kint), intent(in) :: id_file
!!        integer(kind = kint), intent(in) :: num_fld_sq
!!        integer(kind = kint), intent(in) :: num_comp_sq(num_fld_sq)
!!        character(len=kchara), intent(in) :: pwr_name(num_fld_sq)
!!      logical function error_sph_moniter_items(id_file, mode_label,   &
!!     &                                         ntot_comp, item_name)
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len = kchara), intent(in) :: mode_label
!!        integer(kind = kint), intent(in) :: ntot_comp
!!        character(len=kchara), intent(in) :: item_name(ntot_comp)
!!@endverbatim
      module check_sph_monitor_header
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      logical function error_sph_vol_monitor_head(id_file, mode_label,  &
     &                nri_sph, ltr_sph, nlayer_ICB, nlayer_CMB,         &
     &                kr_inside, r_inside, kr_outside, r_outside,       &
     &                num_fld_sq, num_comp_sq, pwr_name,                &
     &                ntot_comp_sq, pwr_label)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: mode_label
!
      integer(kind = kint), intent(in) :: nri_sph, ltr_sph
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: kr_inside, kr_outside
      real(kind = kreal), intent(in) ::   r_inside,  r_outside
      integer(kind = kint), intent(in) :: num_fld_sq, ntot_comp_sq
      integer(kind = kint), intent(in) :: num_comp_sq(num_fld_sq)
      character(len=kchara), intent(in) :: pwr_name(num_fld_sq)
      character (len=kchara), intent(in) :: pwr_label(ntot_comp_sq)
!
      character(len=255) :: character_4_read
      character(len=kchara) :: label(2)
      integer(kind = kint) :: iend = 0
!
!
      call skip_comment(id_file, character_4_read, iend)
      if(iend .gt. 0) then
        error_sph_vol_monitor_head = .TRUE.
        return
      end if
      read(character_4_read, *) label(1:2)
      error_sph_vol_monitor_head                                        &
     &    = error_sph_moniter_two_int(id_file, label, nri_sph, ltr_sph)
      if(error_sph_vol_monitor_head) return
!
!
      call skip_comment(id_file, character_4_read, iend)
      if(iend .gt. 0) then
        error_sph_vol_monitor_head = .TRUE.
        return
      end if
      read(character_4_read, *) label(1:2)
      error_sph_vol_monitor_head                                        &
     &    = error_sph_moniter_two_int(id_file, label,                   &
     &                                nlayer_ICB, nlayer_CMB)
      if(error_sph_vol_monitor_head) return
!
      call skip_comment(id_file, character_4_read, iend)
      if(iend .gt. 0) then
        error_sph_vol_monitor_head = .TRUE.
        return
      end if
      read(character_4_read, *) label(1:2)
      error_sph_vol_monitor_head                                        &
     &    = error_sph_moniter_int_real(id_file, label,                  &
     &                                 kr_inside, r_inside)
      if(error_sph_vol_monitor_head) return
!
      call skip_comment(id_file, character_4_read, iend)
      if(iend .gt. 0) then
        error_sph_vol_monitor_head = .TRUE.
        return
      end if
      read(character_4_read, *) label(1:2)
      error_sph_vol_monitor_head                                        &
     &    = error_sph_moniter_int_real(id_file, label,                  &
     &                                 kr_outside, r_outside)
      if(error_sph_vol_monitor_head) return
!
!
      call skip_comment(id_file, character_4_read, iend)
      if(iend .gt. 0) then
        error_sph_vol_monitor_head = .TRUE.
        return
      end if
      label(1) = 'Number_of_field'
      label(2) = 'Number_of_component'
      error_sph_vol_monitor_head                                        &
     &    = error_sph_moniter_two_int(id_file, label,                   &
     &                           num_fld_sq, ntot_comp_sq)
      if(error_sph_vol_monitor_head) return
!
!
      error_sph_vol_monitor_head                                        &
     &    = error_sph_moniter_ncomp(id_file, num_fld_sq,                &
     &                              num_comp_sq, pwr_name)
      if(error_sph_vol_monitor_head) return
!
      error_sph_vol_monitor_head                                        &
     &     = error_sph_moniter_items(id_file, mode_label,               &
     &                               ntot_comp_sq, pwr_label)
!
      end function error_sph_vol_monitor_head
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      logical function error_sph_moniter_two_int(id_file, label,        &
     &                                           int_ref1, int_ref2)
!
      integer(kind = kint), intent(in) :: id_file
!
      character(len=kchara), intent(in) :: label(2)
      integer(kind = kint), intent(in) :: int_ref1, int_ref2
!
      integer(kind = kint) :: int_read1, int_read2
!
!
      read(id_file,*) int_read1, int_read2
!
      error_sph_moniter_two_int = .FALSE.
      if(int_read1 .ne. int_ref1) then
        write(*,*) trim(label(1)),                                      &
     &             ' does not match between data and file'
        error_sph_moniter_two_int = .TRUE.
        return
      end if
!
      if(int_read2 .ne. int_ref2) then
        write(*,*) trim(label(2)),                                      &
     &             ' does not match between data and file'
        error_sph_moniter_two_int = .TRUE.
        return
      end if
!
      end function error_sph_moniter_two_int
!
!  --------------------------------------------------------------------
!
      logical function error_sph_moniter_int_real(id_file, label,       &
     &                                            int_ref, real_ref)
!
      integer(kind = kint), intent(in) :: id_file
!
      character(len=kchara), intent(in) :: label(2)
      integer(kind = kint), intent(in) :: int_ref
      real(kind = kreal), intent(in) :: real_ref
!
      integer(kind = kint) :: int_read
      real(kind = kreal) :: real_read
!
!
      read(id_file,*) int_read, real_read
!
      error_sph_moniter_int_real = .FALSE.
      if(int_read .ne. int_ref) then
        write(*,*) trim(label(1)),                                      &
     &             ' does not match between data and file'
        error_sph_moniter_int_real = .TRUE.
        return
      end if
!
      if(real(real_read) .ne. real(real_ref)) then
        write(*,*) trim(label(2)),                                      &
     &             ' does not match between data and file'
        error_sph_moniter_int_real = .TRUE.
        return
      end if
!
      end function error_sph_moniter_int_real
!
!  --------------------------------------------------------------------
!
      logical function error_sph_moniter_ncomp(id_file, num_fld_sq,     &
     &                                         num_comp_sq, pwr_name)
!
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint), intent(in) :: num_fld_sq
      integer(kind = kint), intent(in) :: num_comp_sq(num_fld_sq)
      character(len=kchara), intent(in) :: pwr_name(num_fld_sq)
!
      integer(kind = kint), allocatable :: num_read_comp(:)
!
      integer(kind = kint) :: icou
!
!
      allocate(num_read_comp(num_fld_sq))
!
      read(id_file,*) num_read_comp(1:num_fld_sq)
!
      do icou = 1, num_fld_sq
        if(num_read_comp(icou) .ne. num_comp_sq(icou)) then
          write(*,*) 'Number of component for ', trim(pwr_name(icou)),  &
     &               ' does not match with the data file'
          error_sph_moniter_ncomp = .TRUE.
          deallocate(num_read_comp)
          return
        end if
      end do
!
      deallocate(num_read_comp)
      error_sph_moniter_ncomp = .FALSE.
!
      end function error_sph_moniter_ncomp
!
!  --------------------------------------------------------------------
!
      logical function error_sph_moniter_items(id_file, mode_label,     &
     &                                         ntot_comp, item_name)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: mode_label
!
      integer(kind = kint), intent(in) :: ntot_comp
      character(len=kchara), intent(in) :: item_name(ntot_comp)
!
      character (len=kchara), allocatable :: read_name(:)
!
      character(len=kchara) :: tmpc1, tmpc2, tmpc3
      integer(kind = kint) :: icou
!
!
      allocate(read_name(ntot_comp))
!
      if(mode_label .ne. 'EMPTY') then
        read(id_file,*) tmpc1, tmpc2, tmpc3, read_name(1:ntot_comp)
      else
        read(id_file,*) tmpc1, tmpc2, read_name(1:ntot_comp)
      end if
!
      do icou = 1, ntot_comp
        if(cmp_no_case(read_name(icou), item_name(icou))                &
     &                                         .eqv. .FALSE.) then
          write(*,*) 'field ', trim(item_name(icou)),                   &
     &                 ' does not match with the data file',            &
     &                 read_name(icou), item_name(icou)
          error_sph_moniter_items = .TRUE.
          deallocate(read_name)
          return
        end if
      end do
!
      deallocate(read_name)
      error_sph_moniter_items = .FALSE.
!
      end function error_sph_moniter_items
!
!  --------------------------------------------------------------------
!
      end module check_sph_monitor_header
