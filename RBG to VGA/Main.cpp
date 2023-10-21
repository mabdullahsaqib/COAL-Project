#include <iostream>
#include<string.h>
#include <fstream>
#include <vector>
#include <cstdint>

#pragma pack(push, 1) // Ensure there's no padding in the BMP header structure
struct BMPHeader {
    char signature[2];
    std::int32_t fileSize;
    std::int32_t reserved;
    std::int32_t dataOffset;
    std::int32_t headerSize;
    std::int32_t width;
    std::int32_t height;
    std::int16_t planes;
    std::int16_t bitsPerPixel;
    std::int32_t compression;
    std::int32_t imageSize;
    std::int32_t xPixelsPerMeter;
    std::int32_t yPixelsPerMeter;
    std::int32_t colorsUsed;
    std::int32_t colorsImportant;
};
#pragma pack(pop)

int main() {
    // Open the BMP file
    std::string name;
    std::cout << "Enter name of bmp file: ";
    std::cin >> name;
    name += ".bmp";
    std::ifstream bmpFile(name, std::ios::binary);
 
    if (!bmpFile) {
        std::cerr << "Failed to open BMP file." << std::endl;
        return 1;
    }

    // Read the BMP header
    BMPHeader bmpHeader;
    bmpFile.read(reinterpret_cast<char*>(&bmpHeader), sizeof(bmpHeader));

    // Output image metadata, flipped vertically and horizontally
    std::cout << "Image width: " << bmpHeader.width << " pixels" << std::endl;
    std::cout << "Image height: " << bmpHeader.height << " pixels" << std::endl;
    std::cout << "Bits per pixel: " << bmpHeader.bitsPerPixel << std::endl;

    // Calculate the number of bytes in each row (including padding)
    int bytesPerRow = ((bmpHeader.width * bmpHeader.bitsPerPixel + 31) / 32) * 4;

    // Calculate the number of bytes in the image data
    int imageDataSize = bytesPerRow * bmpHeader.height;

    bmpFile.seekg(bmpHeader.dataOffset);
    // Read and output image data (assuming 1 byte per pixel), flipping it
    std::vector<uint8_t> imageData(imageDataSize);
    bmpFile.read(reinterpret_cast<char*>(imageData.data()), imageDataSize);
    int val = 0;
    int x = 0;
    std::ofstream fout;
    fout.open("BinaryArr.txt", std::ios::out);
    if (!fout) {
        std::cout << "Cannot find BinaryArr.txt to write to.";
        return 1;
    }
    for (int row = bmpHeader.height - 1; row >= 0; row--) {
        for (int col = 0; col < bmpHeader.width; col++) {
            int dataIndex = row * bytesPerRow + col;
            val = static_cast<int>(imageData[dataIndex]);
            if (val > 247 && val < 256) {
                fout << 16 << ",";
            }
            else if (val == 0) {
                fout << 255 << ",";
            }
            else
                fout << val << ",";
            ++x;
            if (x >= bmpHeader.width)
            {
                fout << 254;
                fout << ",";
                x = 0;
            }
        }
    }

    // Close the BMP file
    bmpFile.close();

    return 0;
}
