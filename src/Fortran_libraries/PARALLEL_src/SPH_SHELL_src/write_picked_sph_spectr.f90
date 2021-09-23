!>@file   write_picked_sph_spectr.f90
!!@brief  module write_picked_sph_spectr
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays to monitoring spectrum data
!!
!!@verbatim
!!      subroutine write_each_picked_specr_file                         &
!!     &         (time_d, sph_rj, rj_fld, picked)
!!        type(time_data), intent(in) :: time_d
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_data), intent(in) :: rj_fld
!!        type(picked_spectrum_data), intent(in) :: picked
!!      subroutine open_eack_picked_spectr(id_pick, picked, l, m)
!!        integer(kind = kint), intent(in) :: id_pick, l, m
!!        type(picked_spectrum_data), intent(in) :: picked
!!
!!      function picked_each_mode_to_text                               &
!!     &       (i_step, time, radius, kr, l, m, ntot_comp_rj, d_rj_out)
!!@endverbatim
!!
      module write_picked_sph_spectr
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_spheric_parameter
      use t_pickup_sph_spectr_data
      use t_phys_data
      use t_time_data
!
      implicit  none
!
      integer, parameter, private :: len_fixed = 4*16 + 2*25 + 1
!
! -----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_each_picked_specr_file                           &
     &         (time_d, sph_rj, rj_fld, picked)
!
      use pickup_sph_spectr_data
!
      type(time_data), intent(in) :: time_d
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: picked
!
      integer(kind = kint), parameter :: id_pick = 17
      integer(kind = kint) :: inum, knum
      integer(kind = kint_gl) :: num
!
      character(len=kchara) :: fmt_txt
      real(kind=kreal), allocatable :: d_rj_out(:)
!
!
      if(picked%num_sph_mode_lc .le. 0) return
!
      num = picked%istack_picked_spec_lc(my_rank+1)                     &
     &     - picked%istack_picked_spec_lc(my_rank)
      if(num .le. 0) return
!
      allocate(d_rj_out(picked%ntot_comp_rj))
!
      if(picked%idx_out(0,4) .gt. 0) then
        call open_eack_picked_spectr(id_pick, picked, izero, izero)
        call pick_degre0_sped_4_monitor                                 &
     &     (rj_fld, picked, picked%ntot_comp_rj, d_rj_out)
        write(id_pick,'(a)', ADVANCE='NO')                              &
     &     picked_each_mode_to_text                                     &
     &         (time_d%i_time_step, time_d%time, zero, izero,           &
     &          izero, izero, picked%ntot_comp_rj, d_rj_out)
        close(id_pick)
      end if
!
      write(fmt_txt,'(a37,i4,a13)')                                     &
     &         '(i16,1pe25.14e3, i16,1pe25.14e3,2i16,',                 &
     &           picked%ntot_comp_rj, '(1pE25.14e3))'
      do inum = 1, picked%num_sph_mode_lc
        call open_eack_picked_spectr(id_pick, picked,                   &
     &      picked%idx_out(inum,1), picked%idx_out(inum,2))
        do knum = 1, picked%num_layer
          call pick_single_sph_spec_4_monitor(inum, knum,               &
     &        sph_rj, rj_fld, picked, picked%ntot_comp_rj, d_rj_out)
!
          write(id_pick,fmt_txt) time_d%i_time_step, time_d%time,       &
     &        picked%id_radius(knum), picked%radius_gl(knum),           &
     &        picked%idx_out(inum,1:2), d_rj_out(1:picked%ntot_comp_rj)
        end do
        close(id_pick)
      end do
      deallocate(d_rj_out)
!
      end subroutine write_each_picked_specr_file
!
! -----------------------------------------------------------------------
!
      subroutine open_eack_picked_spectr(id_pick, picked, l, m)
!
      use set_parallel_file_name
      use write_field_labels
!
      integer(kind = kint), intent(in) :: id_pick, l, m
      type(picked_spectrum_data), intent(in) :: picked
!
      character(len = kchara) :: file_name, fname_tmp
      integer(kind = kint) :: mm, i
!
      mm = abs(m)
      write(fname_tmp,'(a,a2)') trim(picked%file_prefix), '_l'
      call add_index_after_name(l, fname_tmp, file_name)
      write(fname_tmp,'(a,a2)') trim(file_name), '_m'
      call add_index_after_name(mm, fname_tmp, file_name)
      if(m .lt. 0) then
        write(fname_tmp,'(a,a1)') trim(file_name), 's'
      else
        write(fname_tmp,'(a,a1)') trim(file_name), 'c'
      end if
      file_name = add_dat_extension(fname_tmp)
      open(id_pick, file=file_name, form='formatted', status='old',     &
     &     position='append', err = 99)
      return
!
!
   99 continue
      open(id_pick, file=file_name, form='formatted', status='replace')
!
      write(id_pick,'(a)',ADVANCE='NO')                                 &
     &                       each_pick_sph_header_no_field(picked)
      do i = 1, picked%ntot_comp_rj
        write(id_pick,'(a,a4)',ADVANCE='NO')                            &
     &                          trim(picked%spectr_name(i)), '    '
      end do
      write(id_pick,'(a1)',ADVANCE='NO') char(10)
!
      end subroutine open_eack_picked_spectr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      function picked_each_mode_to_text                                 &
     &       (i_step, time, radius, kr, l, m, ntot_comp_rj, d_rj_out)
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time, radius
      integer(kind = kint), intent(in) :: kr, l, m
      integer(kind = kint), intent(in) :: ntot_comp_rj
      real(kind = kreal), intent(in) :: d_rj_out(ntot_comp_rj)
!
      character(len = len_fixed+ntot_comp_rj*25)                        &
     &                          :: picked_each_mode_to_text
!
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a37,i4,a17)')                                     &
     &         '(i16,1pe25.14e3, i16,1pe25.14e3,2i16,',                 &
     &           ntot_comp_rj, '(1pE25.14e3), a1)'
      write(picked_each_mode_to_text,fmt_txt) i_step, time,             &
     &          kr, radius, l, m, d_rj_out(1:ntot_comp_rj), char(10)
!
      end function  picked_each_mode_to_text
!
! ----------------------------------------------------------------------
!
      end module write_picked_sph_spectr
