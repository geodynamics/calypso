!>@file   compare_sph_monitor_header.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Compare monitor data header
!!
!!@verbatim
!!      logical function cmp_sph_monitor_field_labels                   &
!!     &               (sph_lbl, sph_IN, sph_OUT)
!!      logical function cmp_sph_volume_monitor_heads                   &
!!     &               (sph_lbl_IN, sph_IN, sph_lbl_OUT, sph_OUT)
!!      logical function cmp_sph_layer_monitor_heads                    &
!!     &               (sph_lbl_IN, sph_IN, sph_lbl_OUT, sph_OUT)
!!        type(read_sph_spectr_data), intent(in) :: sph_IN
!!        type(sph_spectr_head_labels), intent(in) :: sph_lbl_IN
!!        type(read_sph_spectr_data), intent(in) :: sph_OUT
!!        type(sph_spectr_head_labels), intent(in) :: sph_lbl_OUT
!!@endverbatim
      module compare_sph_monitor_header
!
      use m_precision
      use m_constants
      use t_sph_spectr_head_labels
      use t_read_sph_spectra
!
      implicit none
!
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      logical function cmp_sph_monitor_field_labels                     &
     &               (sph_lbl, sph_IN, sph_OUT)
!
      use skip_comment_f
!
      type(read_sph_spectr_data), intent(in) :: sph_IN
      type(sph_spectr_head_labels), intent(in) :: sph_lbl
      type(read_sph_spectr_data), intent(in) :: sph_OUT
!
!
      cmp_sph_monitor_field_labels                                      &
       = cmp_sph_monitor_field_names(sph_lbl,                           &
     &               sph_IN%nfield_sph_spec, sph_IN%ntot_sph_spec,      &
     &               sph_IN%num_labels, sph_IN%ncomp_sph_spec,          &
     &               sph_IN%ene_sph_spec_name, sph_OUT%nfield_sph_spec, &
     &               sph_OUT%ntot_sph_spec, sph_OUT%num_labels,         &
     &               sph_OUT%ncomp_sph_spec, sph_OUT%ene_sph_spec_name)
!
      end function cmp_sph_monitor_field_labels
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      logical function cmp_sph_volume_monitor_heads                     &
     &               (sph_lbl_IN, sph_IN, sph_lbl_OUT, sph_OUT)
!
      type(read_sph_spectr_data), intent(in) :: sph_IN
      type(sph_spectr_head_labels), intent(in) :: sph_lbl_IN
      type(read_sph_spectr_data), intent(in) :: sph_OUT
      type(sph_spectr_head_labels), intent(in) :: sph_lbl_OUT
!
!
      cmp_sph_volume_monitor_heads = .FALSE.
      if(cmp_volume_monitor_head_labels(sph_lbl_IN, sph_lbl_OUT)        &
     &    .eqv. .FALSE.) return
      if(cmp_sph_volume_monitor_param(sph_lbl_IN,                       &
     &                               sph_IN%kr_inner,  sph_IN%r_inner,  &
     &                               sph_IN%kr_outer,  sph_IN%r_outer,  &
     &                               sph_OUT%kr_inner, sph_OUT%r_inner, &
     &                               sph_OUT%kr_outer, sph_OUT%r_outer) &
     &   .eqv. .FALSE.) return
!
      cmp_sph_volume_monitor_heads                                      &
     &           = cmp_sph_layer_monitor_heads(sph_lbl_IN, sph_IN,      &
     &                                         sph_lbl_OUT, sph_OUT)
!
      end function cmp_sph_volume_monitor_heads
!
!   --------------------------------------------------------------------
!
      logical function cmp_sph_layer_monitor_heads                      &
     &               (sph_lbl_IN, sph_IN, sph_lbl_OUT, sph_OUT)
!
      type(read_sph_spectr_data), intent(in) :: sph_IN
      type(sph_spectr_head_labels), intent(in) :: sph_lbl_IN
      type(read_sph_spectr_data), intent(in) :: sph_OUT
      type(sph_spectr_head_labels), intent(in) :: sph_lbl_OUT
!
!
      cmp_sph_layer_monitor_heads = .FALSE.
      if(cmp_layer_monitor_head_labels(sph_lbl_IN, sph_lbl_OUT)         &
     &    .eqv. .FALSE.) return
      cmp_sph_layer_monitor_heads                                       &
     &  = cmp_sph_layer_monitor_param(sph_lbl_IN,                       &
     &                                sph_IN%nri_sph,  sph_IN%ltr_sph,  &
     &                                sph_IN%kr_ICB, sph_IN%kr_CMB,     &
     &                                sph_OUT%nri_sph, sph_OUT%ltr_sph, &
     &                                sph_OUT%kr_ICB, sph_OUT%kr_CMB)
!
      end function cmp_sph_layer_monitor_heads
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      logical function cmp_sph_monitor_field_names(sph_lbl_IN,          &
     &       nfield_sph_spec_in,  ntot_sph_spec_in,  num_labels_in,     &
     &       ncomp_sph_spec_in,  ene_sph_spec_name_in,                  &
     &       nfield_sph_spec_out, ntot_sph_spec_out, num_labels_out,    &
     &       ncomp_sph_spec_out, ene_sph_spec_name_out)
!
      use skip_comment_f
!
      type(sph_spectr_head_labels), intent(in) :: sph_lbl_IN
      integer(kind = kint), intent(in) :: nfield_sph_spec_in
      integer(kind = kint), intent(in) :: ntot_sph_spec_in
      integer(kind = kint), intent(in) :: nfield_sph_spec_out
      integer(kind = kint), intent(in) :: ntot_sph_spec_out
      integer(kind = kint), intent(in) :: num_labels_in, num_labels_out
      integer(kind = kint), intent(in)                                  &
     &                       :: ncomp_sph_spec_in(ntot_sph_spec_in)
      integer(kind = kint), intent(in)                                  &
     &                       :: ncomp_sph_spec_out(ntot_sph_spec_out)
      character(len = kchara), intent(in)                               &
     &                       :: ene_sph_spec_name_in(num_labels_in)
      character(len = kchara), intent(in)                               &
     &                       :: ene_sph_spec_name_out(num_labels_out)
!
      integer(kind = kint) :: i
!
!
      cmp_sph_monitor_field_names = .FALSE.
      if(nfield_sph_spec_in .ne. nfield_sph_spec_out) then
        write(*,*) trim(sph_lbl_IN%hdr_num_field), ' does not match',   &
     &      nfield_sph_spec_in, nfield_sph_spec_out
        return
      end if
!
      if(ntot_sph_spec_in .ne. ntot_sph_spec_out) then
        write(*,*) trim(sph_lbl_IN%hdr_num_comp), ' does not match',    &
     &      ntot_sph_spec_in, ntot_sph_spec_out
        return
      end if
!
      do i = 1, nfield_sph_spec_out
        if(ncomp_sph_spec_in(i) .ne. ncomp_sph_spec_out(i)) then
          write(*,*) i, '-th field of ',                                &
     &      trim(sph_lbl_IN%hdr_num_comp), ' does not match',           &
     &      ncomp_sph_spec_in(i), ncomp_sph_spec_out(i)
          return
        end if
      end do
!
      do i = 1, num_labels_out
        if(cmp_no_case(ene_sph_spec_name_out(i),                        &
     &                 ene_sph_spec_name_in(i)) .eqv. .FALSE.) then
          write(*,*) i, '-th label of field label does not match',      &
     &      trim(ene_sph_spec_name_in(i)), ',   ',                      &
     &      trim(ene_sph_spec_name_out(i))
          return
        end if
      end do
      cmp_sph_monitor_field_names = .TRUE.
!
      end function cmp_sph_monitor_field_names
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      logical function cmp_sph_volume_monitor_param(sph_lbl_IN,         &
     &       kr_inner_in,  r_inner_in,  kr_outer_in,  r_outer_in,       &
     &       kr_inner_out, r_inner_out, kr_outer_out, r_outer_out)
!
      use skip_comment_f
!
      type(sph_spectr_head_labels), intent(in) :: sph_lbl_IN
      integer(kind = kint) :: kr_inner_in, kr_outer_in
      real(kind = kreal) :: r_inner_in, r_outer_in
      integer(kind = kint) :: kr_inner_out, kr_outer_out
      real(kind = kreal) :: r_inner_out, r_outer_out
!
!
      cmp_sph_volume_monitor_param = .FALSE.
      if(kr_inner_in .ne. kr_inner_out) then
        write(*,*) trim(sph_lbl_IN%hdr_kr_in), ' does not match',       &
     &      kr_inner_in, kr_inner_out
        return
      end if
!
      if(real(r_inner_in) .ne. real(r_inner_out)) then
        write(*,*) trim(sph_lbl_IN%hdr_r_in), ' does not match',        &
     &      r_inner_in, r_inner_out
        return
      end if
!
      if(kr_outer_in .ne. kr_outer_out) then
        write(*,*) trim(sph_lbl_IN%hdr_kr_out), ' does not match',      &
     &      kr_outer_in, kr_outer_out
        return
      end if
!
      if(real(r_outer_in) .ne. real(r_outer_out)) then
        write(*,*) trim(sph_lbl_IN%hdr_r_out), ' does not match',       &
     &      r_outer_in, r_outer_out
        return
      end if
      cmp_sph_volume_monitor_param = .TRUE.
!
      end function cmp_sph_volume_monitor_param
!
!   --------------------------------------------------------------------
!
      logical function cmp_sph_layer_monitor_param(sph_lbl_IN,          &
     &                nri_sph_in,  ltr_sph_in,  kr_ICB_in,  kr_CMB_in,  &
     &                nri_sph_out, ltr_sph_out, kr_ICB_out, kr_CMB_out)
!
      use skip_comment_f
!
      type(sph_spectr_head_labels), intent(in) :: sph_lbl_IN
!
      integer(kind = kint), intent(in) :: ltr_sph_in, nri_sph_in
      integer(kind = kint), intent(in) :: kr_ICB_in, kr_CMB_in
      integer(kind = kint), intent(in) :: ltr_sph_out, nri_sph_out
      integer(kind = kint), intent(in) :: kr_ICB_out, kr_CMB_out
!
!
      cmp_sph_layer_monitor_param = .FALSE.
      if(nri_sph_in .ne. nri_sph_out) then
        write(*,*) trim(sph_lbl_IN%hdr_nri), ' does not match',         &
     &      nri_sph_in, nri_sph_out
        return
      end if
!
      if(ltr_sph_in .ne. ltr_sph_out) then
        write(*,*) trim(sph_lbl_IN%hdr_ltr), ' does not match',         &
     &      ltr_sph_in, ltr_sph_out
        return
      end if
!
      if(kr_ICB_in .ne. kr_ICB_out) then
        write(*,*) trim(sph_lbl_IN%hdr_ICB_id), ' does not match',      &
     &      kr_ICB_in, kr_ICB_out
        return
      end if
!
      if(kr_CMB_in .ne. kr_CMB_out) then
        write(*,*) trim(sph_lbl_IN%hdr_CMB_id), ' does not match',      &
     &      kr_CMB_in, kr_CMB_out
        return
      end if
      cmp_sph_layer_monitor_param = .TRUE.
!
      end function cmp_sph_layer_monitor_param
!
!   --------------------------------------------------------------------
!
      end module compare_sph_monitor_header
