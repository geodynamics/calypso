!>@file   convert_from_rayleigh_rst.f90
!!@brief  module convert_from_rayleigh_rst
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2018
!
!>@brief  Main loop to assemble spectr data
!!
!!@verbatim
!!      subroutine convert_fields_from_rayleigh(istep, org_fld_file,    &
!!     &          new_sph_mesh, r_itp, ra_rst, new_sph_phys)
!!        type(field_IO_params), intent(in) :: org_fld_file
!!        type(sph_mesh_data), intent(in) :: new_sph_mesh
!!        type(rayleigh_restart), intent(in) :: ra_rst
!!        type(sph_radial_itp_data), intent(in) :: r_itp
!!        type(phys_data), intent(inout) :: new_sph_phys
!!@endverbatim
!
      module convert_from_rayleigh_rst
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use m_machine_parameter
      use m_file_format_switch
!
      use r_interpolate_marged_sph
      use t_SPH_mesh_field_data
      use t_time_data
      use t_field_data_IO
      use t_control_data_4_merge
      use t_control_param_assemble
      use t_spectr_data_4_assemble
      use t_rayleigh_restart_IO
      use t_convert_from_rayleigh
!
      use new_SPH_restart
      use parallel_assemble_sph
      use copy_rj_phys_data_4_IO
      use assemble_sph_fields
      use set_control_newsph
      use rayleigh_restart_IO
      use field_IO_select
!
      implicit none
!
      private :: cvt_each_field_from_rayleigh
      private :: check_chebyshev_trans, check_rayleigh_restart_reading
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine convert_fields_from_rayleigh(istep, org_fld_file,      &
     &          new_sph_mesh, r_itp, ra_rst, new_sph_phys)
!
      use sel_read_rayleigh_restart
!
      integer(kind = kint), intent(in) :: istep
      type(field_IO_params), intent(in) :: org_fld_file
      type(sph_mesh_data), intent(in) :: new_sph_mesh
      type(rayleigh_restart), intent(in) :: ra_rst
      type(sph_radial_itp_data), intent(in) :: r_itp
!
      type(phys_data), intent(inout) :: new_sph_phys
!
      character(len = kchara) :: file_name(2)
      type(work_fftpack_chebyshev) :: fcheby_WK
      type(work_rayleigh_checkpoint) :: rayleigh_WK
!
      integer(kind = kint) :: i_fld, i_comp, nd, iflag_ncomp
      integer(kind = kint) :: ierr
!
!
      call alloc_work_rayleigh_restart                                  &
     &   (ra_rst%nri_org, r_itp, rayleigh_WK)
      call init_fftpack_4_cheby(rayleigh_WK%nri_tgt, fcheby_WK, ierr)
!
      do i_fld = 1, new_sph_phys%num_phys
        if(my_rank .eq. 0) write(*,*) 'set_rayleigh_rst_file_name', i_fld
!
        call set_rayleigh_rst_file_name(ra_rst%i_version,               &
     &      org_fld_file%file_prefix, istep,                            &
     &      new_sph_phys%phys_name(i_fld), iflag_ncomp, file_name(1))
!
        do nd = 1, iflag_ncomp
          i_comp = 2*nd - 1 + new_sph_phys%istack_component(i_fld-1)
!
!          call check_rayleigh_restart_reading(file_name(nd), i_comp,   &
!     &        ra_rst, rayleigh_WK%rayleigh_in)
          call cvt_each_field_from_rayleigh(file_name(nd),              &
     &        i_fld, i_comp, new_sph_mesh,                              &
     &        r_itp, ra_rst, fcheby_WK, rayleigh_WK, new_sph_phys)
        end do
!
      end do
      call dealloc_fftpack_4_cheby(fcheby_WK)
      call dealloc_work_rayleigh_restart(rayleigh_WK)
!
      end subroutine convert_fields_from_rayleigh
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cvt_each_field_from_rayleigh                           &
     &          (file_name, i_fld, i_comp, new_sph_mesh, r_itp, ra_rst, &
     &           fcheby_WK, rayleigh_WK, new_sph_phys)
!
      use m_phys_labels
      use calypso_mpi
      use m_calypso_mpi_IO
      use t_calypso_mpi_IO_param
      use MPI_ascii_data_IO
!
      integer(kind = kint), intent(in) :: i_fld, i_comp
      character(len = kchara), intent(in) :: file_name
      type(sph_mesh_data), intent(in) :: new_sph_mesh
      type(rayleigh_restart), intent(in) :: ra_rst
      type(sph_radial_itp_data), intent(in) :: r_itp
      type(work_fftpack_chebyshev), intent(in) :: fcheby_WK
!
      type(work_rayleigh_checkpoint), intent(inout) :: rayleigh_WK
      type(phys_data), intent(inout) :: new_sph_phys
!
      type(calypso_MPI_IO_params), save :: IO_param
      integer(kind = kint) :: k, j, l, m, ierr
      integer(kind = MPI_OFFSET_KIND) :: ioffset1, ioffset2
      integer(kind = kint_gl), parameter :: ione64 = 1
!
!
      call open_read_mpi_file                                           &
     &   (file_name, nprocs, my_rank, IO_param)
!      write(50+my_rank,*) 'k, kr, l, m, ra_rst%iflag_swap',            &
!       &     new_sph_mesh%sph%sph_rj%nidx_rj(1:2), ra_rst%iflag_swap
      do j = 1, new_sph_mesh%sph%sph_rj%nidx_rj(2)
        l = new_sph_mesh%sph%sph_rj%idx_gl_1d_rj_j(j,2)
        m = new_sph_mesh%sph%sph_rj%idx_gl_1d_rj_j(j,3)
        if(l .gt. ra_rst%ltr_org) cycle
        do k = 1, ra_rst%nri_org
          call find_rayleigh_restart_address                            &
     &       (ra_rst%nri_org, ra_rst%ltr_org,                           &
     &        k, l, abs(m), ioffset1, ioffset2)
!
          call calypso_mpi_seek_read_real                               &
     &       (IO_param%id_file, ra_rst%iflag_swap,                      &
     &        ioffset1, ione64, rayleigh_WK%rayleigh_in(k,1))
          call calypso_mpi_seek_read_real                               &
     &       (IO_param%id_file, ra_rst%iflag_swap,                      &
     &        ioffset2, ione64, rayleigh_WK%rayleigh_in(k,2))
        end do
!
        call rescaling_from_rayleigh                                    &
     &     (l, m, ra_rst%nri_org, rayleigh_WK%rayleigh_in)
!
!      call matmul_bwd_leg_trans(ra_rst%nri_org, ione, ra_rst%nri_org,  &
!     &    ra_rst%Cheby_fwd(1,1), rayleigh_WK%rayleigh_tg(1,1),         &
!     &    rayleigh_WK%rayleigh_fd(1,1))
!
        if    (new_sph_phys%phys_name(i_fld) .eq. velocity%name         &
     &   .or. new_sph_phys%phys_name(i_fld) .eq. pressure%name          &
     &   .or. new_sph_phys%phys_name(i_fld) .eq. temperature%name       &
     &   .or. new_sph_phys%phys_name(i_fld) .eq. magnetic_field%name)   &
     &       then
          call rescaling_for_chebyshev_FFT                              &
     &      (ra_rst%nri_org, rayleigh_WK%rayleigh_in(1,1),              &
     &       rayleigh_WK%nri_tgt, rayleigh_WK%rayleigh_tg(1,1))
          call COST1B(rayleigh_WK%nri_tgt, ione,                        &
     &       rayleigh_WK%rayleigh_tg(1,1), rayleigh_WK%nri_tgt+1,       &
     &       fcheby_WK%WSAVE, fcheby_WK%LENSAV, fcheby_WK%WORK,         &
     &       rayleigh_WK%nri_tgt+1, ierr)
!
          call copy_from_chebyshev_trans(new_sph_mesh%sph%sph_rj,       &
     &        r_itp, j, i_comp,  rayleigh_WK%nri_tgt,                   &
     &        rayleigh_WK%rayleigh_tg(1,1), new_sph_phys)
        else if(new_sph_phys%phys_name(i_fld)                           &
     &                             .eq. previous_momentum%name          &
     &   .or. new_sph_phys%phys_name(i_fld)                             &
     &                             .eq. previous_heat%name              &
     &   .or. new_sph_phys%phys_name(i_fld)                             &
     &                             .eq. previous_induction%name) then
          call radial_interpolation_rayleigh(r_itp,                     &
     &        ra_rst%nri_org, rayleigh_WK%rayleigh_in(1,1),             &
     &        rayleigh_WK%nri_tgt, rayleigh_WK%rayleigh_tg(1,1))
        end if
!
!        call check_chebyshev_trans                                     &
!     &     (new_sph_mesh%sph%sph_rj, r_itp, file_name,                 &
!     &      l, m, j, i_fld, i_comp, ra_rst%nri_org,                    &
!     &      rayleigh_WK%rayleigh_in, rayleigh_WK%nri_tgt,              &
!     &      rayleigh_WK%rayleigh_tg, new_sph_phys)
      end do
!
      call close_mpi_file(IO_param)
!
      end subroutine cvt_each_field_from_rayleigh
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_chebyshev_trans                                  &
     &         (sph_rj, r_itp, file_name, l, m, j, i_fld, i_comp,       &
     &          nri_org, rayleigh_in, nri_tgt, rayleigh_tg,             &
     &          new_sph_phys)
!
      use t_spheric_rj_data
      use t_phys_data
      use r_interpolate_marged_sph
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_radial_itp_data), intent(in) :: r_itp
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: i_fld, i_comp, l, m, j
      integer(kind = kint), intent(in) :: nri_org, nri_tgt
      real(kind = kreal), intent(in) :: rayleigh_in(nri_org,2)
!
      real(kind = kreal), intent(in) :: rayleigh_tg(nri_tgt,1)
      type(phys_data), intent(in) :: new_sph_phys
!
      integer(kind = kint) :: k, kr, inod, iflag
!
!
      iflag = 0
      if(l .eq. 0 .and. m .eq.  0) iflag = 1
      if(l .eq. 1 .and. m .eq.  0) iflag = 1
      if(l .eq. 10 .and. m .eq.  10) iflag = 1
      if(l .eq. 10 .and. m .eq. -10) iflag = 1
      if(iflag .eq. 0) return
!
      write(50+my_rank,*) trim(file_name), l, m
      do k = 1, nri_org
        write(50+my_rank,*) k, rayleigh_in(k,1:2)
      end do
!
      write(50+my_rank,*) 'tgt', trim(file_name), l, m, nri_tgt
      do k = 1, nri_tgt
        kr = r_itp%kr_inner_domain + k - 1
        write(50+my_rank,*) k, sph_rj%radius_1d_rj_r(kr),               &
     &                      rayleigh_tg(k,1)
      end do
!
!      write(50+my_rank,*) 'fld', trim(file_name), l, m, nri_tgt
!      do k = 1, nri_org
!        write(50+my_rank,*) k, rayleigh_fd(k,1)
!      end do
!
      write(50+my_rank,*) trim(new_sph_phys%phys_name(i_fld)), i_comp
      do k = 1, sph_rj%nidx_rj(1)
        inod = j + (k-1) * sph_rj%nidx_rj(2)
        write(50+my_rank,*) k, sph_rj%radius_1d_rj_r(k),                &
     &                      l, m, new_sph_phys%d_fld(inod,i_comp)
      end do
!
      end subroutine check_chebyshev_trans
!
! -----------------------------------------------------------------------
!
      subroutine check_rayleigh_restart_reading                         &
     &         (file_name, i_comp, ra_rst)
!
      use calypso_mpi
      use m_calypso_mpi_IO
      use t_calypso_mpi_IO_param
      use MPI_ascii_data_IO
!
      type(rayleigh_restart), intent(in) :: ra_rst
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: i_comp
!
      type(calypso_MPI_IO_params), save :: IO_param
      integer(kind = MPI_OFFSET_KIND) :: ioffset1, ioffset2
      integer(kind = kint) :: j
      character(len = kchara) :: fn_out
      integer(kind = kint_gl) :: jmax_h
      real(kind = kreal) :: read_fld(2)
      integer(kind = kint_gl), parameter :: ione64 = 1
!
!
      call open_read_mpi_file                                           &
     &   (file_name, nprocs, my_rank, IO_param)
      if(my_rank .eq. 0) then
        write(fn_out,'(a,i1)') 'rayleigh_test.', i_comp
        open(99,file=fn_out)
!
        jmax_h = 1 + ra_rst%ltr_org*(ra_rst%ltr_org+3) / 2
        do j = 1, int(ra_rst%nri_org * jmax_h)
          ioffset1 = (j-1) * kreal
          ioffset2 = ioffset1 + kreal*ra_rst%nri_org*jmax_h
          call calypso_mpi_seek_read_real                               &
     &       (IO_param%id_file, ra_rst%iflag_swap,                      &
     &        ioffset1, ione64, read_fld(1))
          call calypso_mpi_seek_read_real                               &
     &       (IO_param%id_file, ra_rst%iflag_swap,                      &
     &        ioffset2, ione64, read_fld(2))
!
          write(99,*) j, read_fld(1:2)
        end do
!
        close(99)
      end if
      call close_mpi_file(IO_param)
!
      end subroutine check_rayleigh_restart_reading
!
! -----------------------------------------------------------------------
!
      end module convert_from_rayleigh_rst
