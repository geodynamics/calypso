!>@file   sph_monitor_header_text.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!         modified in Sep., 2022
!!
!> @brief Time spectrum data output routines for utilities
!!
!!@verbatim
!!      integer(kind = kint) function len_moniter_i2_head_text          &
!!     &                            (label_1, label_2)
!!        character(len = kchara), intent(in) :: label_1, label_2
!!      function monitor_int2_head_text(len_header, label_1, label_2,   &
!!     &                                  int_param1, int_param2)
!!        integer(kind = kint), intent(in) :: len_header
!!        character(len = kchara), intent(in) :: label_1, label_2
!!        integer(kind = kint), intent(in) :: int_param1, int_param2
!!        character(len = len_header) :: monitor_int2_head_text
!!
!!      integer(kind = kint) function len_moniter_ir_head_text          &
!!     &                            (label_1, label_2)
!!        character(len = kchara), intent(in) :: label_1, label_2
!!      function monitor_ir_head_text(len_header, label_1, label_2,     &
!!     &                              int_param, real_param)
!!        integer(kind = kint), intent(in) :: len_header
!!        character(len = kchara), intent(in) :: label_1, label_2
!!        integer(kind = kint), intent(in) :: int_param
!!        real(kind = kreal), intent(in) :: real_param
!!        character(len = len_header) :: monitor_ir_head_text
!!
!!      integer(kind = kint) function len_monitor_data_ncomps_text      &
!!     &                            (label_fld, label_comp, n_field)
!!        character(len = kchara), intent(in) :: label_fld, label_comp
!!        integer(kind = kint), intent(in) :: n_field
!!      function monitor_data_ncomps_text                               &
!!     &       (len_header, label_fld, label_comp,                      &
!!     &        n_field, ntot_comp, ncomp_sph_spec)
!!        integer(kind = kint), intent(in) :: len_header
!!        character(len = kchara), intent(in) :: label_fld, label_comp
!!        integer(kind = kint), intent(in) :: n_field, ntot_comp
!!        integer(kind = kint), intent(in) :: ncomp_sph_spec(n_field)
!!        character(len = len_header) :: monitor_data_ncomps_text
!!
!!      integer(kind = kint) function len_data_names_text               &
!!     &                            (num_labels, sph_spec_name)
!!        integer(kind = kint), intent(in) :: num_labels
!!        character(len = kchara), intent(in)                           &
!!     &                        :: sph_spec_name(num_labels)
!!      function monitor_data_names_text(len_header,                    &
!!     &                                 num_labels, sph_spec_name)
!!        integer(kind = kint), intent(in) :: len_header, num_labels
!!        character(len = kchara), intent(in)                           &
!!     &                        :: sph_spec_name(num_labels)
!!        character(len = len_header) :: monitor_data_names_text
!!@endverbatim
!
      module sph_monitor_header_text
!
      use m_precision
      use m_constants
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      integer(kind = kint) function len_moniter_i2_head_text            &
     &                            (label_1, label_2)
!
      character(len = kchara), intent(in) :: label_1, label_2
!
      len_moniter_i2_head_text = len_trim(label_1) + 2                  &
     &                          + len_trim(label_2) + 1 + 2*16 + 1
!
      end function len_moniter_i2_head_text
!
!   --------------------------------------------------------------------
!
      function monitor_int2_head_text(len_header, label_1, label_2,     &
     &                                  int_param1, int_param2)
!
      integer(kind = kint), intent(in) :: len_header
      character(len = kchara), intent(in) :: label_1, label_2
      integer(kind = kint), intent(in) :: int_param1, int_param2
!
      character(len = len_header) :: monitor_int2_head_text
!
      write(monitor_int2_head_text,'(3a,a1,2i16,a1)')                   &
     &    trim(label_1), ', ', trim(label_2), char(10),                 &
     &    int_param1, int_param2, char(10)
!
      end function monitor_int2_head_text
!
!   --------------------------------------------------------------------
!
      integer(kind = kint) function len_moniter_ir_head_text            &
     &                            (label_1, label_2)
!
      character(len = kchara), intent(in) :: label_1, label_2
!
      len_moniter_ir_head_text = len_trim(label_1) + 2                  &
     &                          + len_trim(label_2) + 1 + 16 + 25 + 1
!
      end function len_moniter_ir_head_text
!
!   --------------------------------------------------------------------
!
      function monitor_ir_head_text(len_header, label_1, label_2,       &
     &                              int_param, real_param)
!
      integer(kind = kint), intent(in) :: len_header
      character(len = kchara), intent(in) :: label_1, label_2
      integer(kind = kint), intent(in) :: int_param
      real(kind = kreal), intent(in) :: real_param
!
      character(len = len_header) :: monitor_ir_head_text
!
      write(monitor_ir_head_text,'(3a,a1,i16,1pE25.15e3,a1)')           &
     &    trim(label_1), ', ', trim(label_2), char(10),                 &
     &    int_param, real_param, char(10)
!
      end function monitor_ir_head_text
!
!   --------------------------------------------------------------------
!
      integer(kind = kint) function len_monitor_data_ncomps_text        &
     &                            (label_fld, label_comp, n_field)
!
      character(len = kchara), intent(in) :: label_fld, label_comp
      integer(kind = kint), intent(in) :: n_field
!
      len_monitor_data_ncomps_text                                      &
     &   = len_moniter_i2_head_text(label_fld, label_comp)              &
     &    + n_field*5 + (n_field-1) / 16 + 1
!
      end function len_monitor_data_ncomps_text
!
!   --------------------------------------------------------------------
!
      function monitor_data_ncomps_text                                 &
     &       (len_header, label_fld, label_comp,                        &
     &        n_field, ntot_comp, ncomp_sph_spec)
!
      integer(kind = kint), intent(in) :: len_header
      character(len = kchara), intent(in) :: label_fld, label_comp
      integer(kind = kint), intent(in) :: n_field, ntot_comp
      integer(kind = kint), intent(in) :: ncomp_sph_spec(n_field)
!
      character(len = len_header) :: monitor_data_ncomps_text
!
      integer(kind = kint) :: i, ist
!
      ist = len_moniter_i2_head_text(label_fld, label_comp)
      monitor_data_ncomps_text(1:ist)                                   &
     &      = monitor_int2_head_text(ist, label_fld, label_comp,        &
     &                               n_field, ntot_comp)
!
      write(monitor_data_ncomps_text(ist+1:ist+5),'(i5)')               &
     &                                               ncomp_sph_spec(1)
      ist = ist + 5
      do i = 2, n_field
        if(mod((i-1),16) .eq. 0) then
          write(monitor_data_ncomps_text(ist+1:ist+1),'(a1)') char(10)
          ist = ist + 1
        end if
        write(monitor_data_ncomps_text(ist+1:ist+5),'(i5)')             &
     &                                           ncomp_sph_spec(i)
        ist = ist + 5
      end  do
      write(monitor_data_ncomps_text(ist+1:ist+1),'(a1)') char(10)
!
      end function monitor_data_ncomps_text
!
!   --------------------------------------------------------------------
!
      integer(kind = kint) function len_data_names_text                 &
     &                            (num_labels, sph_spec_name)
!
      integer(kind = kint), intent(in) :: num_labels
      character(len = kchara), intent(in)                               &
     &                        :: sph_spec_name(num_labels)
!
      integer(kind = kint) :: i, ist
!
      ist = 0
      do i = 1, num_labels
        ist = ist + len_trim(sph_spec_name(i)) + 4
      end  do
      len_data_names_text = ist + 1
!
      end function len_data_names_text
!
!   --------------------------------------------------------------------
!
      function monitor_data_names_text(len_header,                      &
     &                                 num_labels, sph_spec_name)
!
      integer(kind = kint), intent(in) :: len_header, num_labels
      character(len = kchara), intent(in)                               &
     &                        :: sph_spec_name(num_labels)
      character(len = len_header) :: monitor_data_names_text
!
      integer(kind = kint) :: i, ist, len_name
      character(len = len_header) :: textbuf
!
      ist = 0
      do i = 1, num_labels
        len_name = len_trim(sph_spec_name(i)) + 4
        write(textbuf(ist+1:ist+len_name),'(2a)')                       &
     &            trim(sph_spec_name(i)), '    '
        ist = ist + len_name
      end  do
      write(textbuf(ist+1:ist+1),'(a1)') char(10)
      monitor_data_names_text = textbuf
!
      end function monitor_data_names_text
!
!   --------------------------------------------------------------------
!
      end module sph_monitor_header_text
