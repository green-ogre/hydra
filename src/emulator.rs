use std::{
    fmt::Debug,
    ops::{Index, IndexMut},
};

#[derive(Debug, Clone, Copy)]
enum Reg {
    Zero,
    Ra,
    Sp,
    Gp,
    Tp,
    T(usize),
    S(usize),
    A(usize),
}

impl Reg {
    fn new(reg: u32) -> Self {
        match reg {
            0 => Self::Zero,
            1 => Self::Ra,
            2 => Self::Sp,
            3 => Self::Gp,
            4 => Self::Tp,
            5 => Self::T(0),
            6 => Self::T(1),
            7 => Self::T(2),
            8 => Self::S(0),
            9 => Self::S(1),
            10 => Self::A(0),
            11 => Self::A(1),
            12 => Self::A(2),
            13 => Self::A(3),
            14 => Self::A(4),
            15 => Self::A(5),
            16 => Self::A(6),
            17 => Self::A(7),

            18 => Self::S(2),
            19 => Self::S(3),
            20 => Self::S(4),
            21 => Self::S(5),
            22 => Self::S(6),
            23 => Self::S(7),
            24 => Self::S(8),
            25 => Self::S(9),
            26 => Self::S(10),
            27 => Self::S(11),

            28 => Self::T(3),
            29 => Self::T(4),
            30 => Self::T(5),
            31 => Self::T(6),

            reg => panic!("invalid register: x{reg}"),
        }
    }

    fn reg_index(&self) -> usize {
        match self {
            Reg::Zero => 0,
            Reg::Ra => 1,
            Reg::Sp => 2,
            Reg::Gp => 3,
            Reg::Tp => 4,
            Reg::T(i) => match i {
                0 => 5,
                1 => 6,
                2 => 7,
                3 => 28,
                4 => 29,
                5 => 30,
                6 => 31,
                _ => panic!("invalid t register: {i}"),
            },
            Reg::S(i) => match i {
                0 => 8,
                1 => 9,
                2 => 18,
                3 => 19,
                4 => 20,
                5 => 21,
                6 => 22,
                7 => 23,
                8 => 24,
                9 => 25,
                10 => 26,
                11 => 27,
                _ => panic!("invalid t register: {i}"),
            },
            Reg::A(i) => match i {
                0 => 10,
                1 => 11,
                2 => 12,
                3 => 13,
                4 => 14,
                5 => 15,
                6 => 16,
                7 => 17,
                _ => panic!("invalid t register: {i}"),
            },
        }
    }
}

impl Index<Reg> for [u64; 32] {
    type Output = u64;
    fn index(&self, index: Reg) -> &Self::Output {
        &self[index.reg_index()]
    }
}

impl IndexMut<Reg> for [u64; 32] {
    fn index_mut(&mut self, index: Reg) -> &mut Self::Output {
        &mut self[index.reg_index()]
    }
}

#[derive(Debug, Clone, Copy)]
enum Imm {
    Bin(u32),
    Hex(u32),
}

impl Imm {
    pub const ZERO: Self = Self::Bin(0);

    pub fn val(&self) -> u32 {
        *match self {
            Self::Bin(val) => val,
            Self::Hex(val) => val,
        }
    }

    pub fn val_signed(&self) -> i32 {
        match self {
            Self::Bin(val) => *val as i32,
            Self::Hex(val) => *val as i32,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Offset(Reg, usize);

#[derive(Debug)]
enum Instr {
    Addi(Reg, Reg, Imm),
    Lui(Reg, Imm),
    Auipc(Reg, Imm),
    Slti(Reg, Reg, Imm),
    Sltiu(Reg, Reg, Imm),
    Xori(Reg, Reg, Imm),
    Ori(Reg, Reg, Imm),
    Andi(Reg, Reg, Imm),
    Slli(Reg, Reg, Imm),
    Srli(Reg, Reg, Imm),
    Srai(Reg, Reg, Imm),
    Add(Reg, Reg, Reg),
    Sub(Reg, Reg, Reg),
    Sll(Reg, Reg, Reg),
    Slt(Reg, Reg, Reg),
    Sltu(Reg, Reg, Reg),
    Xor(Reg, Reg, Reg),
    Srl(Reg, Reg, Reg),
    Sra(Reg, Reg, Reg),
    Or(Reg, Reg, Reg),
    And(Reg, Reg, Reg),
    Lb(Reg, Offset),
    Lh(Reg, Offset),
    Lw(Reg, Offset),
    Ld(Reg, Offset),
    Lbu(Reg, Offset),
    Lhu(Reg, Offset),
    Sb(Reg, Offset),
    Sh(Reg, Offset),
    Sw(Reg, Offset),
    Sd(Reg, Offset),
    Jal(Reg, Addr),
    Jalr(Reg, Reg, Addr),
    Beq(Reg, Reg, Addr),
    Bne(Reg, Reg, Addr),
    Blt(Reg, Reg, Addr),
    Bge(Reg, Reg, Addr),
    Bltu(Reg, Reg, Addr),
    Bgeu(Reg, Reg, Addr),
    Ecall,
}

#[derive(Debug, Clone, Copy)]
struct Addr {
    val: u64,
    signed: bool,
}

impl Addr {
    pub const ZERO: Self = Self {
        val: 0,
        signed: false,
    };

    pub fn new_unsigned(val: u64) -> Self {
        Self { val, signed: false }
    }

    pub fn new_signed(val: i64) -> Self {
        Self {
            val: val as u64,
            signed: true,
        }
    }

    pub fn is_signed(&self) -> bool {
        self.signed
    }

    pub fn signed(&self) -> i64 {
        if self.is_signed() {
            println!("retrieved signed value from unsigned addr");
        }

        self.val as i64
    }
}

#[derive(Debug)]
struct Emulator {
    regs: [u64; 32],
    memory_lower: Vec<u8>,
    memory_upper: Vec<u8>,
    pc: u64,

    exiting: bool,
    exit_code: i32,

    stdout: Vec<u8>,
    stderr: Vec<u8>,
}

pub const DRAM_OFFSET: u64 = 4_000_000;
pub const MEM_SIZE: u64 = u64::MAX;
pub const SEG_LEN: u64 = u32::MAX as u64;

impl Default for Emulator {
    fn default() -> Self {
        Self {
            pc: 0,
            memory_lower: vec![0; SEG_LEN as usize],
            memory_upper: vec![0; SEG_LEN as usize],
            regs: Default::default(),
            exit_code: 0,
            exiting: false,
            stdout: Vec::new(),
            stderr: Vec::new(),
        }
    }
}

impl Emulator {
    /// Load program data into memory at offset [`Addr`].
    ///
    /// Subsequently sets pc to offset. Ignores sign.
    pub fn flash_prgm(&mut self, prgm: &[u8], offset: Addr) {
        println!("flashing program...");

        for (byte, b) in self
            .memory_mut(offset, prgm.len())
            .iter_mut()
            .zip(prgm.iter())
        {
            *byte = *b;
        }
        self.pc = offset.val;
    }

    /// Run program until the exit ecall is made.
    pub fn run(&mut self) {
        loop {
            let raw_instr = self.read_pc();
            if !self.step(raw_instr as u32) {
                return;
            }
        }
    }

    /// Run program until iterations is reached or the exit ecall made.
    pub fn run_for(&mut self, mut iterations: usize) {
        loop {
            let raw_instr = self.read_pc();
            if iterations == 0 {
                return;
            }

            if !self.step(raw_instr as u32) {
                return;
            }

            iterations -= 1;
        }
    }

    /// Slice of memory at location [`Addr`].
    ///
    /// Sign of offset is ignored.
    fn memory(&self, offset: Addr, len: usize) -> &[u8] {
        let offset = offset.val as usize;

        if offset + len < SEG_LEN as usize {
            &self.memory_lower[offset..offset + len]
        } else if offset + len < SEG_LEN as usize * 2 && offset >= SEG_LEN as usize {
            let offset = offset - SEG_LEN as usize;
            &self.memory_upper[offset..offset + len]
        } else {
            panic!("segfault");
            // &[
            //     &self.memory_lower[offset..],
            //     &self.memory_upper[..offset - SEG_LEN as usize + len],
            // ]
            // .concat()
        }
    }

    /// Slice of mutable memory at location [`Addr`].
    ///
    /// Sign of offset is ignored.
    fn memory_mut(&mut self, offset: Addr, len: usize) -> &mut [u8] {
        let offset = offset.val as usize;

        if offset + len < SEG_LEN as usize {
            &mut self.memory_lower[offset..offset + len]
        } else if offset + len < SEG_LEN as usize * 2 && offset >= SEG_LEN as usize {
            let offset = offset - SEG_LEN as usize;
            &mut self.memory_upper[offset..offset + len]
        } else {
            panic!("segfault");
            // &[
            //     &self.memory_lower[offset..],
            //     &self.memory_upper[..offset - SEG_LEN as usize + len],
            // ]
            // .concat()
        }
    }

    fn step(&mut self, raw_instr: u32) -> bool {
        if self.exiting {
            println!("exiting: {}", self.exit_code);
            return false;
        }

        println!("fetching instr: {raw_instr:#x}");

        let opcode = raw_instr & 0b1111111;
        let instr = match opcode {
            // R-type
            0x33 => {
                let rd = (raw_instr >> 7) & 0b11111;
                let fn3 = (raw_instr >> 12) & 0b111;
                let rs1 = (raw_instr >> 15) & 0b11111;
                let rs2 = (raw_instr >> 20) & 0b11111;
                let fn7 = raw_instr >> 25;
                println!("decoded instr: {fn7:#x} {rs2:#x} {rs1:#x} {fn3:#x} {rd:#x} {opcode:#x}");

                let rd = Reg::new(rd);
                let rs1 = Reg::new(rs1);
                let rs2 = Reg::new(rs2);

                match fn7 {
                    0b0000000 => match fn3 {
                        0b000 => Instr::Add(rd, rs1, rs2),
                        0b001 => Instr::Sll(rd, rs1, rs2),
                        0b010 => Instr::Slt(rd, rs1, rs2),
                        0b011 => Instr::Sltu(rd, rs1, rs2),
                        0b100 => Instr::Xor(rd, rs1, rs2),
                        0b101 => Instr::Srl(rd, rs1, rs2),
                        0b110 => Instr::Or(rd, rs1, rs2),
                        0b111 => Instr::And(rd, rs1, rs2),
                        _ => unreachable!(),
                    },
                    0b0100000 => match fn3 {
                        0b000 => Instr::Sub(rd, rs1, rs2),
                        0b101 => Instr::Sra(rd, rs1, rs2),
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                }
            }
            // I-type
            0x03 | 0x67 | 0x13 => {
                let rd = (raw_instr >> 7) & 0b11111;
                let fn3 = (raw_instr >> 12) & 0b111;
                let rs1 = (raw_instr >> 15) & 0b11111;
                let imm = raw_instr >> 20;

                // For shifting ops
                let shamt = (raw_instr >> 20) & 0b11111;
                let fn7 = (raw_instr >> 25) & 0b1111111;

                let imm = if imm & 0x800 != 0 {
                    (imm as i32) | 0xFFFFF000u32 as i32
                } else {
                    imm as i32
                };

                println!("decoded instr: {imm:#x} {rs1:#x} {fn3:#x} {rd:#x} {opcode:#x}");

                let rd = Reg::new(rd);
                let rs1 = Reg::new(rs1);
                let shamt = Imm::Bin(shamt);

                match opcode {
                    0b0000011 => match fn3 {
                        0b000 => Instr::Lb(rd, Offset(rs1, imm as usize)),
                        0b001 => Instr::Lh(rd, Offset(rs1, imm as usize)),
                        0b010 => Instr::Lw(rd, Offset(rs1, imm as usize)),
                        0b100 => Instr::Lbu(rd, Offset(rs1, imm as usize)),
                        0b101 => Instr::Lhu(rd, Offset(rs1, imm as usize)),
                        _ => unreachable!(),
                    },
                    0b0010011 => match fn3 {
                        0b000 => Instr::Addi(rd, rs1, Imm::Bin(imm as u32)),
                        0b010 => Instr::Slti(rd, rs1, Imm::Bin(imm as u32)),
                        0b011 => Instr::Sltiu(rd, rs1, Imm::Bin(imm as u32)),
                        0b100 => Instr::Xori(rd, rs1, Imm::Bin(imm as u32)),
                        0b110 => Instr::Ori(rd, rs1, Imm::Bin(imm as u32)),
                        0b111 => Instr::Andi(rd, rs1, Imm::Bin(imm as u32)),
                        0b001 => Instr::Slli(rd, rs1, shamt),
                        0b101 => match fn7 {
                            0b0100000 => Instr::Srai(rd, rs1, shamt),
                            0b0000000 => Instr::Srli(rd, rs1, shamt),
                            _ => unreachable!(),
                        },
                        _ => panic!("{fn3}"),
                    },
                    0b1100111 => match fn3 {
                        0b000 => Instr::Jalr(rd, rs1, Addr::new_signed(imm as i64)),
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                }
            }
            // S-type
            0x23 => {
                let imm1 = (raw_instr >> 7) & 0b11111;
                let fn3 = (raw_instr >> 12) & 0b111;
                let rs1 = (raw_instr >> 15) & 0b11111;
                let rs2 = (raw_instr >> 20) & 0b11111;
                let imm2 = (raw_instr >> 25) & 0b1111111;

                let imm = imm1 | imm2;
                let imm = if imm & 0x800 != 0 {
                    (imm as i32) | 0xFFFFF000u32 as i32
                } else {
                    imm as i32
                };

                println!("decoded instr: {imm:#x} {rs2:#x} {rs1:#x} {fn3:#x} {opcode:#x}");

                let rs1 = Reg::new(rs1);
                let rs2 = Reg::new(rs2);
                let offset = Offset(rs1, imm as usize);

                match fn3 {
                    0b000 => Instr::Sb(rs2, offset),
                    0b001 => Instr::Sh(rs2, offset),
                    0b010 => Instr::Sw(rs2, offset),
                    _ => unreachable!(),
                }
            }
            // B-type
            0x63 => {
                let imm1 = (raw_instr >> 7) & 0b1;
                let imm2 = (raw_instr >> 8) & 0b1111;
                let fn3 = (raw_instr >> 12) & 0b111;
                let rs1 = (raw_instr >> 15) & 0b11111;
                let rs2 = (raw_instr >> 20) & 0b11111;
                let imm3 = (raw_instr >> 25) & 0b111111;
                let imm4 = (raw_instr >> 31) & 0b1;

                let imm = (imm2 << 1) | (imm3 << 1) | (imm1 << 11) | imm4 << 31;

                println!("decoded instr: {imm:#x} {rs2:#x} {rs1:#x} {fn3:#x} {opcode:#x}");

                let rs1 = Reg::new(rs1);
                let rs2 = Reg::new(rs2);
                let addr = Addr::new_signed(imm as i64);

                match fn3 {
                    0b000 => Instr::Beq(rs1, rs2, addr),
                    0b001 => Instr::Bne(rs1, rs2, addr),
                    0b100 => Instr::Blt(rs1, rs2, addr),
                    0b101 => Instr::Bge(rs1, rs2, addr),
                    0b110 => Instr::Bltu(rs1, rs2, addr),
                    0b111 => Instr::Bgeu(rs1, rs2, addr),
                    _ => unreachable!(),
                }
            }
            // U-type
            0x37 | 0x17 => {
                let rd = (raw_instr >> 7) & 0b11111;
                let imm = (raw_instr >> 12) << 12;

                println!("decoded instr: {imm:#x} {rd:#x} {opcode:#x}");

                let rd = Reg::new(rd);
                let imm = Imm::Bin(imm as u32);

                match opcode {
                    0b0110111 => Instr::Lui(rd, imm),
                    0b0010111 => Instr::Auipc(rd, imm),
                    _ => unreachable!(),
                }
            }
            // J-type
            0x6F => {
                let rd = (raw_instr >> 7) & 0b11111;
                let imm1 = (raw_instr >> 12) & 0b11111111;
                let imm2 = (raw_instr >> 20) & 0b1;
                let imm3 = (raw_instr >> 21) & 0b1111111111;
                let imm4 = (raw_instr >> 31) & 0b1;

                let imm = (imm3 << 1) | (imm2 << 11) | (imm1 << 12) | (imm4 << 31);

                println!("decoded instr: {imm:#x} {rd:#x} {opcode:#x}");

                let rd = Reg::new(rd);
                let addr = Addr::new_signed(imm as i64);

                Instr::Jal(rd, addr)
            }
            0b1110011 => Instr::Ecall,
            opcode => panic!("invalid opcode: {:x}", opcode),
        };

        self.execute(instr);
        true
    }

    pub fn set(&mut self, reg: Reg, val: u64) {
        self.regs[reg] = val;
    }

    pub fn set_signed(&mut self, reg: Reg, val: i64) {
        self.regs[reg] = val as u64;
    }

    pub fn reg(&self, reg: Reg) -> u64 {
        self.regs[reg]
    }

    pub fn reg_signed(&self, reg: Reg) -> i64 {
        self.regs[reg] as i64
    }

    pub fn add_pc(&mut self, offset: Addr) {
        if offset.is_signed() {
            self.pc = (self.pc as i64 + offset.signed()) as u64;
        } else {
            self.pc = self.pc + offset.val;
        }
    }

    pub fn read_pc(&self) -> u64 {
        self.load(Offset(Reg::Zero, self.pc as usize), 4)
    }

    pub fn load(&self, offset: Offset, bytes: usize) -> u64 {
        let mut val = 0;
        let offset = self.reg(offset.0) as usize + offset.1;
        let memory = self.memory(Addr::new_unsigned(offset as u64), bytes);
        for (i, byte) in memory.iter().enumerate() {
            val += (*byte as u64) << (i * 8);
        }

        val
    }

    pub fn load_signed(&self, offset: Offset, bytes: usize) -> i64 {
        let mut val = 0;
        let offset = self.reg(offset.0) as usize + offset.1;
        let memory = self.memory(Addr::new_unsigned(offset as u64), bytes);
        for (i, byte) in memory.iter().enumerate() {
            val += (*byte as i64) << (i * 8);
        }
        val
    }

    pub fn store(&mut self, offset: Offset, bytes: usize, val: u64) {
        let offset = self.reg(offset.0) as usize + offset.1;
        let memory = self.memory_mut(Addr::new_unsigned(offset as u64), bytes);
        for (i, byte) in memory.iter_mut().enumerate() {
            *byte = (val >> (i * 8)) as u8;
        }
    }

    fn execute(&mut self, instr: Instr) {
        println!("executing: {instr:?}");

        self.set(Reg::Zero, 0);

        match instr {
            Instr::Lui(dst, imm) => {
                self.set_signed(dst, ((imm.val_signed() as i64) >> 12) << 12);
            }
            Instr::Auipc(dst, imm) => {
                self.set_signed(
                    dst,
                    self.pc as i64 + (((imm.val_signed() as i64) >> 12) << 12),
                );
            }
            Instr::Addi(dst, src, imm) => {
                self.set_signed(dst, self.reg_signed(src) + imm.val_signed() as i64);
            }
            Instr::Slti(dst, src, imm) => {
                self.set(
                    dst,
                    if self.reg_signed(src) < imm.val_signed() as i64 {
                        1
                    } else {
                        0
                    },
                );
            }
            Instr::Sltiu(dst, src, imm) => {
                self.set(
                    dst,
                    if self.reg(src) < imm.val() as u64 {
                        1
                    } else {
                        0
                    },
                );
            }
            Instr::Xori(dst, src, imm) => {
                self.set_signed(dst, self.reg_signed(src) ^ (imm.val_signed() as i64));
            }
            Instr::Ori(dst, src, imm) => {
                self.set_signed(dst, self.reg_signed(src) | (imm.val_signed() as i64));
            }
            Instr::Andi(dst, src, imm) => {
                self.set_signed(dst, self.reg_signed(src) & (imm.val_signed() as i64));
            }
            Instr::Slli(dst, src, imm) => {
                self.set_signed(dst, self.reg_signed(src) << (imm.val_signed() & 0b11111));
            }
            Instr::Srli(dst, src, imm) => {
                self.set_signed(dst, self.reg_signed(src) >> (imm.val() & 0b11111));
            }
            Instr::Srai(dst, src, imm) => {
                self.set_signed(dst, self.reg_signed(src) >> (imm.val_signed() & 0b11111));
            }
            Instr::Add(dst, src1, src2) => {
                self.set(dst, self.reg(src1) + self.reg(src2));
            }
            Instr::Sub(dst, src1, src2) => {
                self.set_signed(dst, self.reg_signed(src1) - self.reg_signed(src2));
            }
            Instr::Sll(dst, src1, src2) => {
                self.set(dst, self.reg(src1) << (self.reg(src2) & 0b11111));
            }
            Instr::Slt(dst, src1, src2) => {
                self.set(
                    dst,
                    if self.reg_signed(src1) < self.reg_signed(src2) {
                        1
                    } else {
                        0
                    },
                );
            }
            Instr::Sltu(dst, src1, src2) => {
                self.set(
                    dst,
                    if self.reg(src1) < self.reg(src2) {
                        1
                    } else {
                        0
                    },
                );
            }
            Instr::Xor(dst, src1, src2) => {
                self.set(dst, self.reg(src1) ^ self.reg(src2));
            }
            Instr::Srl(dst, src1, src2) => {
                self.set(dst, self.reg(src1) >> (self.reg(src2) & 0b11111));
            }
            Instr::Sra(dst, src1, src2) => {
                self.set_signed(
                    dst,
                    self.reg_signed(src1) >> (self.reg_signed(src2) & 0b11111),
                );
            }
            Instr::Or(dst, src1, src2) => {
                self.set(dst, self.reg(src1) | self.reg(src2));
            }
            Instr::And(dst, src1, src2) => {
                self.set(dst, self.reg(src1) & self.reg(src2));
            }
            Instr::Lb(dst, offset) => {
                self.set_signed(dst, self.load_signed(offset, 1));
            }
            Instr::Lh(dst, offset) => {
                self.set_signed(dst, self.load_signed(offset, 2));
            }
            Instr::Lw(dst, offset) => {
                self.set_signed(dst, self.load_signed(offset, 4));
            }
            Instr::Ld(dst, offset) => {
                self.set_signed(dst, self.load_signed(offset, 8));
            }
            Instr::Lbu(dst, offset) => {
                self.set(dst, self.load(offset, 1));
            }
            Instr::Lhu(dst, offset) => {
                self.set(dst, self.load(offset, 2));
            }
            Instr::Sb(src, offset) => {
                self.store(offset, 1, self.reg(src));
            }
            Instr::Sh(src, offset) => {
                self.store(offset, 2, self.reg(src));
            }
            Instr::Sw(src, offset) => {
                self.store(offset, 4, self.reg(src));
            }
            Instr::Sd(src, offset) => {
                self.store(offset, 8, self.reg(src));
            }
            Instr::Jal(dst, offset) => {
                self.set(dst, self.pc + 4);
                self.add_pc(offset);
            }
            Instr::Jalr(dst, src, offset) => {
                self.execute(Instr::Addi(Reg::T(1), Reg::Zero, Imm::Bin(self.pc as u32)));
                self.execute(Instr::Addi(dst, Reg::T(1), Imm::Bin(4)));
                let jmp = ((self.reg_signed(src) + offset.signed()) & !1) as u64;
                self.pc = jmp;
            }
            Instr::Beq(src1, src2, offset) => {
                if self.reg(src1) == self.reg(src2) {
                    self.add_pc(offset);
                } else {
                    self.add_pc(Addr::new_unsigned(4));
                }
            }
            Instr::Bne(src1, src2, offset) => {
                if self.reg(src1) != self.reg(src2) {
                    self.add_pc(offset);
                } else {
                    self.add_pc(Addr::new_unsigned(4));
                }
            }
            Instr::Blt(src1, src2, offset) => {
                if self.reg_signed(src1) < self.reg_signed(src2) {
                    self.add_pc(offset);
                } else {
                    self.add_pc(Addr::new_unsigned(4));
                }
            }
            Instr::Bge(src1, src2, offset) => {
                if self.reg_signed(src1) >= self.reg_signed(src2) {
                    self.add_pc(offset);
                } else {
                    self.add_pc(Addr::new_unsigned(4));
                }
            }
            Instr::Bltu(src1, src2, offset) => {
                if self.reg(src1) < self.reg(src2) {
                    self.add_pc(offset);
                } else {
                    self.add_pc(Addr::new_unsigned(4));
                }
            }
            Instr::Bgeu(src1, src2, offset) => {
                if self.reg(src1) >= self.reg(src2) {
                    self.add_pc(offset);
                } else {
                    self.add_pc(Addr::new_unsigned(4));
                }
            }
            Instr::Ecall => {
                let syscall = self.reg(Reg::A(7));
                match syscall {
                    // Exit
                    93 => {
                        self.exiting = true;
                        self.exit_code = self.reg_signed(Reg::A(0)) as i32;
                    }
                    // Write
                    64 => match self.reg(Reg::A(0)) {
                        1 => {
                            let buf_addr = self.reg(Reg::A(1)) as usize;
                            let buf_len = self.reg(Reg::A(2)) as usize;
                            println!("writing {} bytes of buf {} to stdout.", buf_len, buf_addr);
                            let memory = self
                                .memory(Addr::new_unsigned(buf_addr as u64), buf_len)
                                .to_vec();
                            self.stdout.extend_from_slice(&memory);
                        }
                        _ => unimplemented!(),
                    },
                    val => println!("invalid syscall: {}", val),
                }
            }
        }

        match instr {
            Instr::Bgeu(_, _, _)
            | Instr::Bltu(_, _, _)
            | Instr::Bge(_, _, _)
            | Instr::Blt(_, _, _)
            | Instr::Bne(_, _, _)
            | Instr::Beq(_, _, _)
            | Instr::Jal(_, _)
            | Instr::Jalr(_, _, _) => {}
            _ => {
                self.add_pc(Addr::new_unsigned(4));
            }
        }
    }
}

pub fn emulate(prgm: &[u8]) {
    let mut emulator = Emulator::default();
    emulator.flash_prgm(prgm, Addr::new_unsigned(DRAM_OFFSET as u64));
    emulator.run();

    for (i, byte) in emulator.memory(Addr::ZERO, 8).iter().enumerate() {
        println!("mem-{} \t{:#04x}", i, byte);
    }

    for i in 0..32 {
        println!("x{} \t{:#018x}", i, emulator.regs[i]);
    }

    println!("stdout: {}", String::from_utf8_lossy(&emulator.stdout));
}

/// https://github.com/d0iasm/rvemu/blob/main/tests/rv32i.rs
#[cfg(test)]
mod tests {
    use super::*;

    const REGISTERS_COUNT: usize = 32;

    /// Create registers for x0-x31 with expected values.
    pub fn create_xregs(non_zero_regs: Vec<(usize, u64)>) -> [u64; REGISTERS_COUNT] {
        let mut xregs = [0; REGISTERS_COUNT];

        // Based on XRegisters::new().
        // xregs[2] = DEFAULT_SP;
        xregs[2] = 0;
        // xregs[11] = POINTER_TO_DTB;

        for pair in non_zero_regs.iter() {
            xregs[pair.0] = pair.1;
        }
        xregs
    }

    /// Start a test and check if the registers are expected.
    pub fn run(emulator: &mut Emulator, data: Vec<u8>, expected_xregs: &[u64; 32]) {
        emulator.flash_prgm(&data, Addr::new_unsigned(0));
        emulator.run_for(data.len() / 4);

        for (i, e) in expected_xregs.iter().enumerate() {
            if *e != 0 {
                assert_eq!(*e, emulator.regs[i], "fails at {}", i);
            }
        }
    }

    #[test]
    fn lb_rd_offset_rs1() {
        let mut emu = Emulator::default();

        let data = vec![
            0x13, 0x08, 0x50, 0x00, // addi x16, x0, 5
            0x93, 0x08, 0x30, 0x00, // addi x17, x0, 3
            0x03, 0x09, 0x40, 0x00, // lb x18, 4(x0)
        ];
        let expected_xregs = create_xregs(vec![(16, 5), (17, 3), (18, 0x93)]);

        run(&mut emu, data, &expected_xregs);
    }

    #[test]
    fn lh_rd_offset_rs1() {
        let mut emu = Emulator::default();

        let data = vec![
            0x13, 0x08, 0x50, 0x00, // addi x16, x0, 5
            0x93, 0x08, 0x30, 0x00, // addi x17, x0, 3
            0x03, 0x19, 0x40, 0x00, // lh x18, 4(x0)
        ];
        let expected_xregs = create_xregs(vec![(16, 5), (17, 3), (18, 0x0893)]);

        run(&mut emu, data, &expected_xregs);
    }

    #[test]
    fn lw_rd_offset_rs1() {
        let mut emu = Emulator::default();

        let data = vec![
            0x13, 0x08, 0x50, 0x00, // addi x16, x0, 5
            0x93, 0x08, 0x30, 0x00, // addi x17, x0, 3
            0x03, 0x29, 0x40, 0x00, // lw x18, 4(x0)
        ];
        let expected_xregs = create_xregs(vec![(16, 5), (17, 3), (18, 0x300893)]);

        run(&mut emu, data, &expected_xregs);
    }

    #[test]
    fn lbu_rd_offset_rs1() {
        let mut emu = Emulator::default();

        let data = vec![
            0x13, 0x08, 0x50, 0x00, // addi x16, x0, 5
            0x93, 0x08, 0x30, 0x00, // addi x17, x0, 3
            0x03, 0x49, 0x40, 0x00, // lbu x18, 4(x0)
        ];
        let expected_xregs = create_xregs(vec![(16, 5), (17, 3), (18, 0x93)]);

        run(&mut emu, data, &expected_xregs);
    }

    #[test]
    fn lhu_rd_offset_rs1() {
        let mut emu = Emulator::default();

        let data = vec![
            0x13, 0x08, 0x50, 0x00, // addi x16, x0, 5
            0x93, 0x08, 0x30, 0x00, // addi x17, x0, 3
            0x03, 0x59, 0x40, 0x00, // lbu x18, 4(x0)
        ];
        let expected_xregs = create_xregs(vec![(16, 5), (17, 3), (18, 0x0893)]);

        run(&mut emu, data, &expected_xregs);
    }

    #[test]
    fn addi_rd_rs1_imm() {
        let mut emu = Emulator::default();

        let data = vec![
            0x93, 0x0f, 0x40, 0x00, // addi x31, x0, 4
        ];
        let expected_xregs = create_xregs(vec![(31, 4)]);

        run(&mut emu, data, &expected_xregs);
    }

    #[test]
    fn slli_rd_rs1_imm() {
        let mut emu = Emulator::default();

        let data = vec![
            0x13, 0x08, 0x20, 0x00, // addi x16 x0, 2
            0x93, 0x18, 0x38, 0x00, // slli x17, x16, 3
        ];
        let expected_xregs = create_xregs(vec![(16, 2), (17, 16)]);

        run(&mut emu, data, &expected_xregs);
    }

    #[test]
    fn slti_rd_rs1_imm() {
        let mut emu = Emulator::default();

        let data = vec![
            0x13, 0x08, 0xb0, 0xff, // addi x16 x0, -5
            0x93, 0x28, 0xe8, 0xff, // slti x17, x16, -2
        ];
        let expected_xregs = create_xregs(vec![(16, -5i64 as u64), (17, 1)]);

        run(&mut emu, data, &expected_xregs);
    }

    #[test]
    fn sltiu_rd_rs1_imm() {
        let mut emu = Emulator::default();

        let data = vec![
            0x13, 0x08, 0x20, 0x00, // addi x16, x0, 2
            0x93, 0x38, 0x58, 0x00, // sltiu, x17, x16, 5
        ];
        let expected_xregs = create_xregs(vec![(16, 2), (17, 1)]);

        run(&mut emu, data, &expected_xregs);
    }

    #[test]
    fn xori_rd_rs1_imm() {
        let mut emu = Emulator::default();

        let data = vec![
            0x13, 0x08, 0x30, 0x00, // addi x16, x0, 3
            0x93, 0x48, 0x68, 0x00, // xori, x17, x16, 6
        ];
        let expected_xregs = create_xregs(vec![(16, 3), (17, 5)]);

        run(&mut emu, data, &expected_xregs);
    }

    #[test]
    fn srai_rd_rs1_imm() {
        let mut emu = Emulator::default();

        let data = vec![
            0x13, 0x08, 0x80, 0xff, // addi x16, x0, -8
            0x93, 0x58, 0x28, 0x40, // srai x17, x16, 2
        ];
        let expected_xregs = create_xregs(vec![(16, -8i64 as u64), (17, -2i64 as u64)]);

        run(&mut emu, data, &expected_xregs);
    }

    #[test]
    fn srli_rd_rs1_imm() {
        let mut emu = Emulator::default();

        let data = vec![
            0x13, 0x08, 0x80, 0x00, // addi x16, x0, 8
            0x93, 0x58, 0x28, 0x00, // srli x17, x16, 2
        ];
        let expected_xregs = create_xregs(vec![(16, 8), (17, 2)]);

        run(&mut emu, data, &expected_xregs);
    }

    #[test]
    fn ori_rd_rs1_imm() {
        let mut emu = Emulator::default();

        let data = vec![
            0x13, 0x08, 0x30, 0x00, // addi x16, x0, 3
            0x93, 0x68, 0x68, 0x00, // ori, x17, x16, 6
        ];
        let expected_xregs = create_xregs(vec![(16, 3), (17, 7)]);

        run(&mut emu, data, &expected_xregs);
    }

    #[test]
    fn andi_rd_rs1_imm() {
        let mut emu = Emulator::default();

        let data = vec![
            0x13, 0x08, 0x40, 0x00, // addi x16, x0, 4
            0x93, 0x78, 0x78, 0x00, // andi, x17, x16, 7
        ];
        let expected_xregs = create_xregs(vec![(16, 4), (17, 4)]);

        run(&mut emu, data, &expected_xregs);
    }

    #[test]
    fn auipc_rd_imm() {
        let mut emu = Emulator::default();

        let data = vec![
            0x17, 0x28, 0x00, 0x00, // auipc x16, 2
        ];
        let expected_xregs = create_xregs(vec![(16, 0x2000)]);

        run(&mut emu, data, &expected_xregs);
    }

    #[test]
    fn sb_rs2_offset_rs1() {
        let mut emu = Emulator::default();

        let data = vec![
            0x13, 0x08, 0xb0, 0xff, // addi x16, x0, -5
            0x93, 0x08, 0x30, 0x00, // addi x17, x0, 3
            0x23, 0x02, 0x00, 0x01, // sb x16, 4(x0)
            0x03, 0x09, 0x40, 0x00, // lb x18, 4(x0)
        ];
        let expected_xregs = create_xregs(vec![
            (16, -5i64 as u64),
            (17, 3),
            (18, ((-5i64 as u64) << 56) >> 56),
        ]);

        run(&mut emu, data, &expected_xregs);
    }

    #[test]
    fn sh_rs2_offset_rs1() {
        let mut emu = Emulator::default();

        let data = vec![
            0x13, 0x08, 0x00, 0xc0, // addi x16, x0, -1024
            0x93, 0x08, 0x30, 0x00, // addi x17, x0, 3
            0x23, 0x12, 0x00, 0x01, // sh x16, 4(x0)
            0x03, 0x19, 0x40, 0x00, // lh x18, 4(x0)
        ];
        let expected_xregs = create_xregs(vec![
            (16, -1024i64 as u64),
            (17, 3),
            (18, ((-1024i64 as u64) << 48) >> 48),
        ]);

        run(&mut emu, data, &expected_xregs);
    }

    #[test]
    fn sw_rs2_offset_rs1() {
        let mut emu = Emulator::default();

        let data = vec![
            0x13, 0x08, 0x00, 0x80, // addi x16, x0, -2048
            0x93, 0x08, 0x30, 0x00, // addi x17, x0, 3
            0x23, 0x22, 0x00, 0x01, // sw x16, 4(x0)
            0x03, 0x29, 0x40, 0x00, // lw x18, 4(x0)
        ];
        let expected_xregs = create_xregs(vec![
            (16, -2048i64 as u64),
            (17, 3),
            (18, ((-2048i64 as u64) << 32) >> 32),
        ]);

        run(&mut emu, data, &expected_xregs);
    }

    #[test]
    fn add_rd_rs1_rs2() {
        let mut emu = Emulator::default();

        let data = vec![
            0x93, 0x01, 0x50, 0x00, // addi x3, x0, 5
            0x13, 0x02, 0x60, 0x00, // addi x4, x0, 6
            0x33, 0x81, 0x41, 0x00, // add x2, x3, x4
        ];
        let expected_xregs = create_xregs(vec![(2, 11), (3, 5), (4, 6)]);

        run(&mut emu, data, &expected_xregs);
    }

    #[test]
    fn sub_rd_rs1_rs2() {
        let mut emu = Emulator::default();

        let data = vec![
            0x93, 0x01, 0x50, 0x00, // addi x3, x0, 5
            0x13, 0x02, 0x60, 0x00, // addi x4, x0, 6
            0x33, 0x81, 0x41, 0x40, // sub x2, x3, x4
        ];
        let expected_xregs = create_xregs(vec![(2, -1i64 as u64), (3, 5), (4, 6)]);

        run(&mut emu, data, &expected_xregs);
    }

    #[test]
    fn sll_rd_rs1_rs2() {
        let mut emu = Emulator::default();

        let data = vec![
            0x13, 0x08, 0x80, 0x00, // addi x16, x0, 8
            0x93, 0x08, 0x20, 0x00, // addi x17, x0, 2
            0x33, 0x19, 0x18, 0x01, // sll x18, x16, x17
        ];
        let expected_xregs = create_xregs(vec![(16, 8), (17, 2), (18, 32)]);

        run(&mut emu, data, &expected_xregs);
    }

    #[test]
    fn slt_rd_rs1_rs2() {
        let mut emu = Emulator::default();

        let data = vec![
            0x13, 0x08, 0x80, 0xff, // addi x16, x0, -8
            0x93, 0x08, 0x20, 0x00, // addi x17, x0, 2
            0x33, 0x29, 0x18, 0x01, // slt x18, x16, x17
        ];
        let expected_xregs = create_xregs(vec![(16, -8i64 as u64), (17, 2), (18, 1)]);

        run(&mut emu, data, &expected_xregs);
    }

    #[test]
    fn sltu_rd_rs1_rs2() {
        let mut emu = Emulator::default();

        let data = vec![
            0x13, 0x08, 0x80, 0x00, // addi x16, x0, 8
            0x93, 0x08, 0x20, 0x00, // addi x17, x0, 2
            0x33, 0xb9, 0x08, 0x01, // slt x18, x17, x16
        ];
        let expected_xregs = create_xregs(vec![(16, 8), (17, 2), (18, 1)]);

        run(&mut emu, data, &expected_xregs);
    }

    #[test]
    fn xor_rd_rs1_rs2() {
        let mut emu = Emulator::default();

        let data = vec![
            0x13, 0x08, 0x30, 0x00, // addi x16, x0, 3
            0x93, 0x08, 0x60, 0x00, // addi x17, x0, 6
            0x33, 0x49, 0x18, 0x01, // xor x18, x16, x17
        ];
        let expected_xregs = create_xregs(vec![(16, 3), (17, 6), (18, 5)]);

        run(&mut emu, data, &expected_xregs);
    }

    #[test]
    fn srl_rd_rs1_rs2() {
        let mut emu = Emulator::default();

        let data = vec![
            0x13, 0x08, 0x00, 0x01, // addi x16, x0, 16
            0x93, 0x08, 0x20, 0x00, // addi x17, x0, 2
            0x33, 0x59, 0x18, 0x01, // srl x18, x16, x17
        ];
        let expected_xregs = create_xregs(vec![(16, 16), (17, 2), (18, 4)]);

        run(&mut emu, data, &expected_xregs);
    }

    #[test]
    fn sra_rd_rs1_rs2() {
        let mut emu = Emulator::default();

        let data = vec![
            0x13, 0x08, 0x00, 0xff, // addi x16, x0, -16
            0x93, 0x08, 0x20, 0x00, // addi x17, x0, 2
            0x33, 0x59, 0x18, 0x41, // sra x18, x16, x17
        ];
        let expected_xregs = create_xregs(vec![(16, -16i64 as u64), (17, 2), (18, -4i64 as u64)]);

        run(&mut emu, data, &expected_xregs);
    }

    #[test]
    fn or_rd_rs1_rs2() {
        let mut emu = Emulator::default();

        let data = vec![
            0x13, 0x08, 0x30, 0x00, // addi x16, x0, 3
            0x93, 0x08, 0x50, 0x00, // addi x17, x0, 5
            0x33, 0x69, 0x18, 0x01, // or x18, x16, x17
        ];
        let expected_xregs = create_xregs(vec![(16, 3), (17, 5), (18, 7)]);

        run(&mut emu, data, &expected_xregs);
    }

    #[test]
    fn and_rd_rs1_rs2() {
        let mut emu = Emulator::default();

        let data = vec![
            0x13, 0x08, 0x30, 0x00, // addi x16, x0, 3
            0x93, 0x08, 0x50, 0x00, // addi x17, x0, 5
            0x33, 0x79, 0x18, 0x01, // and x18, x16, x17
        ];
        let expected_xregs = create_xregs(vec![(16, 3), (17, 5), (18, 1)]);

        run(&mut emu, data, &expected_xregs);
    }

    #[test]
    fn lui_rd_imm() {
        let mut emu = Emulator::default();

        let data = vec![
            0x37, 0x28, 0x00, 0x00, // lui x16, 2
        ];
        let expected_xregs = create_xregs(vec![(16, 8192)]);

        run(&mut emu, data, &expected_xregs);
    }

    #[test]
    fn beq_rs1_rs2_imm() {
        let mut emu = Emulator::default();

        let data = vec![
            0x13, 0x08, 0x30, 0x00, // addi x16, x0, 3
            0x93, 0x08, 0x30, 0x00, // addi x17, x0, 3
            0x63, 0x06, 0x18, 0x01, // beq x16, x17, 12
        ];
        let expected_xregs = create_xregs(vec![(16, 3), (17, 3)]);

        run(&mut emu, data, &expected_xregs);

        assert_eq!(20, emu.pc);
    }

    #[test]
    fn bne_rs1_rs2_imm() {
        let mut emu = Emulator::default();

        let data = vec![
            0x13, 0x08, 0x30, 0x00, // addi x16, x0, 3
            0x93, 0x08, 0x50, 0x00, // addi x17, x0, 5
            0x63, 0x16, 0x18, 0x01, // bne x16, x17, 12
        ];
        let expected_xregs = create_xregs(vec![(16, 3), (17, 5)]);

        run(&mut emu, data, &expected_xregs);

        assert_eq!(20, emu.pc);
    }

    #[test]
    fn blt_rs1_rs2_imm() {
        let mut emu = Emulator::default();

        let data = vec![
            0x13, 0x08, 0xd0, 0xff, // addi x16, x0, -3
            0x93, 0x08, 0x50, 0x00, // addi x17, x0, 5
            0x63, 0x46, 0x18, 0x01, // blt x16, x17, 12
        ];
        let expected_xregs = create_xregs(vec![(16, -3i64 as u64), (17, 5)]);

        run(&mut emu, data, &expected_xregs);

        assert_eq!(20, emu.pc);
    }

    #[test]
    fn bge_rs1_rs2_imm() {
        let mut emu = Emulator::default();

        let data = vec![
            0x13, 0x08, 0xd0, 0xff, // addi x16, x0, -3
            0x93, 0x08, 0xd0, 0xff, // addi x17, x0, -3
            0x63, 0x56, 0x18, 0x01, // bge x16, x17, 12
        ];
        let expected_xregs = create_xregs(vec![(16, -3i64 as u64), (17, -3i64 as u64)]);

        run(&mut emu, data, &expected_xregs);

        assert_eq!(20, emu.pc);
    }

    #[test]
    fn bltu_rs1_rs2_imm() {
        let mut emu = Emulator::default();

        let data = vec![
            0x13, 0x08, 0x30, 0x00, // addi x16, x0, 3
            0x93, 0x08, 0x50, 0x00, // addi x17, x0, 5
            0x63, 0x66, 0x18, 0x01, // bltu x16, x17, 12
        ];
        let expected_xregs = create_xregs(vec![(16, 3), (17, 5)]);

        run(&mut emu, data, &expected_xregs);

        assert_eq!(20, emu.pc);
    }

    #[test]
    fn bgeu_rs1_rs2_imm() {
        let mut emu = Emulator::default();

        let data = vec![
            0x13, 0x08, 0x50, 0x00, // addi x16, x0, 5
            0x93, 0x08, 0x30, 0x00, // addi x17, x0, 3
            0x63, 0x76, 0x18, 0x01, // bgeu x16, x17, 12
        ];
        let expected_xregs = create_xregs(vec![(16, 5), (17, 3)]);

        run(&mut emu, data, &expected_xregs);

        assert_eq!(20, emu.pc);
    }

    #[test]
    fn jalr_rd_imm() {
        let mut emu = Emulator::default();

        let data = vec![
            0x13, 0x08, 0x30, 0x00, // addi x16, x0, 3
            0x93, 0x08, 0x50, 0x00, // addi x17, x0, 5
            0x67, 0x09, 0xc0, 0x02, // jalr x18, x0, 44
        ];
        let expected_xregs = create_xregs(vec![(16, 3), (17, 5), (18, 12)]);

        run(&mut emu, data, &expected_xregs);

        assert_eq!(44, emu.pc);
    }

    #[test]
    fn jal_rd_imm() {
        let mut emu = Emulator::default();

        let data = vec![
            0x13, 0x08, 0x30, 0x00, // addi x16, x0, 3
            0x93, 0x08, 0x50, 0x00, // addi x17, x0, 5
            0x6f, 0x09, 0xc0, 0x00, // jal x18, 12
        ];
        let expected_xregs = create_xregs(vec![(16, 3), (17, 5), (18, 12)]);

        run(&mut emu, data, &expected_xregs);

        assert_eq!(20, emu.pc);
    }
}
