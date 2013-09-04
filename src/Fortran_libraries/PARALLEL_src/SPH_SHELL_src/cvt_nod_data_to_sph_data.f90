!cvt_nod_data_to_sph_data.f90
!     module cvt_nod_data_to_sph_data
!
!      Written by H. Matsui on Feb., 2008
!
!      subroutine cvt_xyz_to_sph_vec_sph_data
!      subroutine cvt_xyz_from_sph_vec_sph_data
!
!      subroutine cvt_xyz_to_sph_tensor_data
!      subroutine cvt_sph_to_xyz_tensor_data
!
!      subroutine copy_nod_scalar_to_sph_data
!      subroutine copy_nod_scalar_from_sph_data
!
!      subroutine copy_nod_vector_to_sph_data
!      subroutine copy_nod_vector_from_sph_data
!
      module cvt_nod_data_to_sph_data
!
      use m_precision
!
      use m_machine_parameter
      use m_node_phys_data
      use m_sph_spectr_data
!
      implicit  none
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine cvt_xyz_to_sph_vec_sph_data
!
      use cvt_nod_fld_and_sph_fld
!
      integer(kind = kint) :: i, j, j0, i_field, i_rtp
!
!
      do j = 1, num_vector_rtp
        j0 = istart_vector_rtp + j - 1
        i_rtp =   istack_phys_comp_rtp(j0-1) + 1
        do i = 1, num_nod_phys
          if (phys_name_rtp(j0) .eq. phys_nod_name(i)) then
!
            i_field = istack_nod_component(i-1) + 1
            call cvt_vec_fld_to_sph_vec(i_rtp, i_field)
            exit
!
          end if
        end do
      end do
!
      end subroutine cvt_xyz_to_sph_vec_sph_data
!
! -------------------------------------------------------------------
!
      subroutine cvt_xyz_from_sph_vec_sph_data
!
      use cvt_nod_fld_and_sph_fld
!
!
      integer(kind = kint) :: i, j, j0, i_field, i_rtp
!
!
      do j = 1, num_vector_rtp
        j0 = istart_vector_rtp + j - 1
        i_rtp =   istack_phys_comp_rtp(j0-1) + 1
        do i = 1, num_nod_phys
          if (phys_name_rtp(j0) .eq. phys_nod_name(i)) then
!
            i_field = istack_nod_component(i-1) + 1
            call cvt_sph_vec_to_vec_fld(i_rtp, i_field)
            exit
!
          end if
        end do
      end do
!
      end subroutine cvt_xyz_from_sph_vec_sph_data
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine cvt_xyz_to_sph_tensor_data
!
      use cvt_nod_fld_and_sph_fld
!
      integer(kind = kint) :: i, j, j0, i_field, i_rtp
!
!
      do j = 1, num_tensor_rtp
        j0 = istart_tensor_rtp + j - 1
        i_rtp =   istack_phys_comp_rtp(j0-1) + 1
        do i = 1, num_nod_phys
          if (phys_name_rtp(j0) .eq. phys_nod_name(i)) then
!
            i_field = istack_nod_component(i-1) + 1
            call cvt_tsr_fld_to_sph_tsr(i_rtp, i_field)
            exit
!
          end if
        end do
      end do
!
      end subroutine cvt_xyz_to_sph_tensor_data
!
! -------------------------------------------------------------------
!
      subroutine cvt_sph_to_xyz_tensor_data
!
      use cvt_nod_fld_and_sph_fld
!
      integer(kind = kint) :: i, j, j0, i_field, i_rtp
!
!
      do j = 1, num_tensor_rtp
        j0 = istart_tensor_rtp + j - 1
        i_rtp =   istack_phys_comp_rtp(j0-1) + 1
        do i = 1, num_nod_phys
          if (phys_name_rtp(j0) .eq. phys_nod_name(i)) then
!
            i_field = istack_nod_component(i-1) + 1
            call cvt_sph_tsr_to_tsr_fld(i_rtp, i_field)
            exit
!
          end if
        end do
      end do
!
      end subroutine cvt_sph_to_xyz_tensor_data
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_nod_scalar_to_sph_data
!
      use cvt_nod_fld_and_sph_fld
!
      integer(kind = kint) :: i, j, j0, i_field, i_rtp
!
!
      do j = 1, num_scalar_rtp
        j0 = istart_scalar_rtp + j - 1
        i_rtp =   istack_phys_comp_rtp(j0-1) + 1
        do i = 1, num_nod_phys
          if (phys_name_rtp(j0) .eq. phys_nod_name(i)) then
!
            i_field = istack_nod_component(i-1) + 1
            call copy_scl_fld_2_sph_vector(i_rtp, i_field)
            exit
          end if
        end do
      end do
!
      end subroutine copy_nod_scalar_to_sph_data
!
! -------------------------------------------------------------------
!
      subroutine copy_nod_scalar_from_sph_data
!
      use cvt_nod_fld_and_sph_fld
!
      integer(kind = kint) :: i, j, j0, i_field, i_rtp
!
!
      do j = 1, num_scalar_rtp
        j0 = istart_scalar_rtp + j - 1
        i_rtp =   istack_phys_comp_rtp(j0-1) + 1
        do i = 1, num_nod_phys
          if (phys_name_rtp(j0) .eq. phys_nod_name(i)) then
!
            i_field = istack_nod_component(i-1) + 1
            call copy_sph_scalar_2_scl_fld(i_rtp, i_field)
            exit
          end if
        end do
      end do
!
      end subroutine copy_nod_scalar_from_sph_data
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine copy_nod_vector_to_sph_data
!
      use cvt_nod_fld_and_sph_fld
!
      integer(kind = kint) :: i, j, j0, i_field, i_rtp
!
!
      do j = 1, num_vector_rtp
        j0 = istart_vector_rtp + j - 1
        i_rtp =   istack_phys_comp_rtp(j0-1) + 1
        do i = 1, num_nod_phys
          if (phys_name_rtp(j0) .eq. phys_nod_name(i)) then
!
            i_field = istack_nod_component(i-1) + 1
            call copy_vct_fld_2_sph_vector(i_rtp, i_field)
            exit
!
          end if
        end do
      end do
!
      end subroutine copy_nod_vector_to_sph_data
!
! -------------------------------------------------------------------
!
      subroutine copy_nod_vector_from_sph_data
!
      use cvt_nod_fld_and_sph_fld
!
      integer(kind = kint) :: i, j, j0, i_field, i_rtp
!
!
      do j = 1, num_vector_rtp
        j0 = istart_vector_rtp + j - 1
        i_rtp =   istack_phys_comp_rtp(j0-1) + 1
        do i = 1, num_nod_phys
          if (phys_name_rtp(j0) .eq. phys_nod_name(i)) then
!
            i_field = istack_nod_component(i-1) + 1
            call copy_sph_vector_2_vec_fld(i_rtp, i_field)
            exit
!
          end if
        end do
      end do
!
      end subroutine copy_nod_vector_from_sph_data
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      end module cvt_nod_data_to_sph_data
