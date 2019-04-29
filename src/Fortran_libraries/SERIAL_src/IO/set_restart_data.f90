!> @file  set_restart_data.f90
!!      module set_restart_data
!!
!! @author  H. Matsui
!! @date Programmed in 2001
!
!> @brief Copy between field data and IO data
!!
!!@verbatim
!!      subroutine count_field_num_to_rst_IO(num_fld, phys_name,        &
!!     &          num_fld_IO)
!!      subroutine copy_field_name_to_rst_IO (nnod, num_fld,            &
!!     &          istack_comp, phys_name, num_fld_IO, ntot_comp_IO,     &
!!     &          ncomp_IO, istack_comp_IO, field_IO_name, nnod_IO)
!!      subroutine copy_field_data_to_rst_IO(nnod, num_fld, ntot_comp,  &
!!     &          istack_comp, phys_name, d_nod,                        &
!!     &          num_fld_IO, ntot_comp_IO, istack_comp_IO,             &
!!     &          field_IO_name, nnod_IO, dat_IO)
!!
!!      subroutine simple_copy_fld_name_to_rst_IO                       &
!!     &         (num_fld, istack_comp, phys_name, num_fld_IO,          &
!!     &          ntot_comp_IO, ncomp_IO, istack_comp_IO, field_IO_name)
!!      subroutine simple_copy_fld_dat_to_rst_IO(nnod, ntot_comp, d_nod,&
!!     &          ntot_comp_IO, nnod_IO, dat_IO)
!!
!!      subroutine copy_field_data_from_rst_IO(nnod, num_fld, ntot_comp,&
!!     &          istack_comp, phys_name, d_nod,                        &
!!     &          num_fld_IO, ntot_comp_IO, istack_comp_IO,             &
!!     &          field_IO_name, nnod_IO, dat_IO)
!!
!!      subroutine set_output_field_flag_4_rst(d_name, iflag)
!!      subroutine set_num_comps_4_rst(rst_name, numdir)
!!
!!      subroutine set_field_id_4_read_rst(d_name, num_fld, istack_comp,&
!!     &          phys_name, numdir, i_field)
!!@endverbatim
!
      module set_restart_data
!
      use m_precision
!
      implicit  none
! 
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine count_field_num_to_rst_IO(num_fld, phys_name,          &
     &          num_fld_IO)
!
      integer(kind=kint), intent(in)  :: num_fld
      character (len=kchara), intent(in) :: phys_name(num_fld)
!
      integer(kind=kint), intent(inout) :: num_fld_IO
!
      integer(kind=kint)  :: i, iflag
!
!
      num_fld_IO = 0
      do i = 1, num_fld
        call set_output_field_flag_4_rst(phys_name(i), iflag)
        num_fld_IO = num_fld_IO + iflag
      end do
!
      end subroutine count_field_num_to_rst_IO
!
!------------------------------------------------------------------
!
      subroutine copy_field_name_to_rst_IO (nnod, num_fld, istack_comp, &
     &          phys_name, num_fld_IO, ntot_comp_IO, ncomp_IO,          &
     &          istack_comp_IO, field_IO_name, nnod_IO)
!
      integer(kind=kint), intent(in)  :: nnod, num_fld
      integer(kind=kint), intent(in)  :: istack_comp(0:num_fld)
      character(len=kchara), intent(in) :: phys_name(num_fld)
!
      integer(kind=kint), intent(in) :: num_fld_IO
!
      integer(kind=kint), intent(inout) :: nnod_IO, ntot_comp_IO
      integer(kind=kint), intent(inout) :: ncomp_IO(num_fld_IO)
      integer(kind=kint), intent(inout) :: istack_comp_IO(0:num_fld_IO)
      character(len=kchara), intent(inout) :: field_IO_name(num_fld_IO)
!
      integer(kind=kint)  :: i, iflag, icou
!
!
      icou = 0
      istack_comp_IO(0) = icou
      do i = 1, num_fld
        call set_output_field_flag_4_rst(phys_name(i), iflag)
!
        if(iflag .gt. 0) then
          icou = icou + 1
          field_IO_name(icou) = phys_name(i)
          ncomp_IO(icou) = istack_comp(i) - istack_comp(i-1)
          istack_comp_IO(icou) = istack_comp_IO(icou-1)               &
     &                           + ncomp_IO(icou)
        end if
      end do
      ntot_comp_IO = istack_comp_IO(icou)
      nnod_IO =      nnod
!
      end subroutine copy_field_name_to_rst_IO
!
!------------------------------------------------------------------
!
      subroutine copy_field_data_to_rst_IO(nnod, num_fld, ntot_comp,    &
     &          istack_comp, phys_name, d_nod,                          &
     &          num_fld_IO, ntot_comp_IO, istack_comp_IO,               &
     &          field_IO_name, nnod_IO, dat_IO)
!
      integer(kind=kint), intent(in)  :: nnod, num_fld, ntot_comp
      integer(kind=kint), intent(in)  :: istack_comp(0:num_fld)
      character(len=kchara), intent(in) :: phys_name(num_fld)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      integer(kind=kint), intent(in) :: num_fld_IO, ntot_comp_IO
      character(len=kchara), intent(in) :: field_IO_name(num_fld_IO)
      integer(kind=kint), intent(in) :: istack_comp_IO(0:num_fld_IO)
      integer(kind=kint), intent(in) :: nnod_IO
!
      real(kind = kreal), intent(inout) :: dat_IO(nnod_IO,ntot_comp_IO)
!
      integer(kind=kint)  :: i, j, numdir, inod
      integer(kind=kint)  :: ist_rst, ist_nod, nd
!
!
      do j = 1, num_fld_IO
        do i = 1, num_fld
          if (phys_name(i) .eq. field_IO_name(j)) then
            ist_nod = istack_comp(i-1)
            ist_rst = istack_comp_IO(j-1)
            numdir =  istack_comp(i) - istack_comp(i-1)
!$omp parallel private(inod)
            do nd = 1, numdir
!$omp do
              do inod = 1, nnod
                dat_IO(inod,ist_rst+nd) = d_nod(inod,ist_nod+nd)
              end do
!$omp end do nowait
            end do
!$omp end parallel
            exit
          end if
        end do
      end do
!
      end subroutine copy_field_data_to_rst_IO
!
!------------------------------------------------------------------
!
      subroutine simple_copy_fld_name_to_rst_IO                         &
     &         (num_fld, istack_comp, phys_name, num_fld_IO,            &
     &          ntot_comp_IO, ncomp_IO, istack_comp_IO, field_IO_name)
!
      integer(kind=kint), intent(in)  :: num_fld
      integer(kind=kint), intent(in)  :: istack_comp(0:num_fld)
      character(len=kchara), intent(in) :: phys_name(num_fld)
!
      integer(kind=kint), intent(in) :: num_fld_IO
      character(len=kchara), intent(inout) :: field_IO_name(num_fld_IO)
      integer(kind=kint), intent(inout) :: ncomp_IO(num_fld_IO)
      integer(kind=kint), intent(inout) :: istack_comp_IO(0:num_fld_IO)
      integer(kind=kint), intent(inout) :: ntot_comp_IO
!
      integer(kind=kint)  :: i
!
!
      istack_comp_IO(0) = istack_comp(0)
      do i = 1, num_fld
        field_IO_name(i) =  phys_name(i)
        ncomp_IO(i) =       istack_comp(i) - istack_comp(i-1)
        istack_comp_IO(i) = istack_comp(i)
      end do
      ntot_comp_IO = istack_comp_IO(num_fld)
!
      end subroutine simple_copy_fld_name_to_rst_IO
!
!------------------------------------------------------------------
!
      subroutine simple_copy_fld_dat_to_rst_IO(nnod, ntot_comp, d_nod,  &
     &          ntot_comp_IO, nnod_IO, dat_IO)
!
      integer(kind=kint), intent(in)  :: nnod, ntot_comp
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      integer(kind=kint), intent(in) :: nnod_IO, ntot_comp_IO
      real(kind = kreal), intent(inout) :: dat_IO(nnod_IO,ntot_comp_IO)
!
!
!$omp workshare
      dat_IO(1:nnod,1:ntot_comp) = d_nod(1:nnod,1:ntot_comp)
!$omp end workshare
!
      end subroutine simple_copy_fld_dat_to_rst_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_field_data_from_rst_IO(nnod, num_fld, ntot_comp,  &
     &          istack_comp, phys_name, d_nod,                          &
     &          num_fld_IO, ntot_comp_IO, istack_comp_IO,               &
     &          field_IO_name, nnod_IO, dat_IO)
!
      use m_phys_labels
!
      integer(kind=kint), intent(in) :: num_fld_IO, ntot_comp_IO
      character(len=kchara), intent(in) :: field_IO_name(num_fld_IO)
      integer(kind=kint), intent(in) :: istack_comp_IO(0:num_fld_IO)
      integer(kind=kint), intent(in) :: nnod_IO
      real(kind = kreal), intent(in) :: dat_IO(nnod_IO,ntot_comp_IO)
!
      integer(kind=kint), intent(in)  :: nnod, num_fld, ntot_comp
      integer(kind=kint), intent(in)  :: istack_comp(0:num_fld)
      character(len=kchara), intent(in) :: phys_name(num_fld)
!
      real(kind = kreal), intent(inout) :: d_nod(nnod,ntot_comp)
!
      integer(kind=kint)  :: i, j, numdir, inod
      integer(kind=kint)  :: ist_rst, ist_nod, nd
!
!
      do j = 1, num_fld_IO
        do i = 1, num_fld
          if (phys_name(i) .eq. field_IO_name(j)) then
            ist_nod = istack_comp(i-1)
            ist_rst = istack_comp_IO(j-1)
            numdir =  istack_comp(i) - istack_comp(i-1)
!$omp parallel private(inod)
            do nd = 1, numdir
!$omp do
              do inod = 1, nnod
                d_nod(inod,ist_nod+nd) = dat_IO(inod,ist_rst+nd)
              end do
!$omp end do nowait
            end do
!$omp end parallel
            exit
          end if
        end do
      end do
!
      end subroutine copy_field_data_from_rst_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_output_field_flag_4_rst(d_name, iflag)
!
      use m_phys_labels
!
      character (len=kchara), intent(in) :: d_name
      integer(kind = kint), intent(inout) :: iflag
!
!
      iflag = 0
      if    (d_name .eq. fhd_velo                                       &
     &  .or. d_name .eq. fhd_press                                      &
     &  .or. d_name .eq. fhd_pre_mom) then
        iflag = 1
        return
      end if
!
      if    (d_name .eq. fhd_vecp                                       &
     &  .or. d_name .eq. fhd_magne                                      &
     &  .or. d_name .eq. fhd_mag_potential                              &
     &  .or. d_name .eq. fhd_pre_uxb) then
        iflag = 1
        return
      end if
!
      if    (d_name .eq. fhd_temp                                       &
     &  .or. d_name .eq. fhd_pre_heat                                   &
     &  .or. d_name .eq. fhd_heat_source) then
        iflag = 1
        return
      end if
!
      if    (d_name .eq. fhd_light                                      &
     &  .or. d_name .eq. fhd_pre_composit                               &
     &  .or. d_name .eq. fhd_light_source) then
        iflag = 1
        return
      end if
!
      end subroutine set_output_field_flag_4_rst
!
!------------------------------------------------------------
!
      subroutine set_num_comps_4_rst(rst_name, numdir)
!
      use m_phys_labels
!
      integer(kind = kint), intent(inout) :: numdir
      character(len=kchara), intent(in) :: rst_name
!
!
      if (      rst_name .eq. fhd_velo                                  &
     &     .or. rst_name .eq. fhd_vecp                                  &
     &     .or. rst_name .eq. fhd_magne                                 &
     &     .or. rst_name .eq. fhd_pre_mom                               &
     &     .or. rst_name .eq. fhd_pre_uxb                               &
     &     .or. rst_name .eq. fhd_chk_mom                               &
     &     .or. rst_name .eq. fhd_chk_uxb   ) then
        numdir = 3
!
      else if  (rst_name .eq. fhd_press                                 &
     &     .or. rst_name .eq. fhd_temp                                  &
     &     .or. rst_name .eq. fhd_light                                 &
     &     .or. rst_name .eq. fhd_mag_potential                         &
     &     .or. rst_name .eq. fhd_pre_heat                              &
     &     .or. rst_name .eq. fhd_pre_composit                          &
     &     .or. rst_name .eq. fhd_chk_heat                              &
     &     .or. rst_name .eq. fhd_chk_composit                          &
     &     .or. rst_name .eq. fhd_chk_press                             &
     &     .or. rst_name .eq. fhd_chk_potential    ) then
        numdir = 1
      end if
!
      end subroutine set_num_comps_4_rst
!
!------------------------------------------------------------
!
      subroutine set_field_id_4_read_rst(d_name, num_fld, istack_comp,  &
     &          phys_name, numdir, i_field)
!
      character (len=kchara), intent(in) :: d_name
      integer(kind=kint), intent(in)  :: num_fld
      integer(kind=kint), intent(in)  :: istack_comp(0:num_fld)
      character (len=kchara), intent(in) :: phys_name(num_fld)
!
      integer(kind = kint), intent(inout) :: numdir, i_field
!
      integer(kind=kint)  :: j
!
!
      numdir =  0
      i_field = 0
      do j = 1, num_fld
        if(d_name .eq. phys_name(j)) then
          i_field = istack_comp(j-1) + 1
          numdir =  istack_comp(j) - istack_comp(j-1)
          exit
        end if
      end do
!
      end subroutine set_field_id_4_read_rst
!
!------------------------------------------------------------
!
      end module set_restart_data
