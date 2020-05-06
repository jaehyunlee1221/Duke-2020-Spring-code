import setuptools

with open("README.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
    name="STA-663-Final-Project-SSVD", # Replace with your own username
    version="0.1.2",
    author="Jae Hyun Lee, Jingxuan Zhang",
    author_email="jaehyun.lee@duke.edu, jingxuan.zhang@duke.edu",
    description="Optimized Sparse Singular Value Decomposition with Numba",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/jaehyunlee1221/STA-663-Final-Project/SSVD",
    packages=setuptools.find_packages(),
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
    ],
    py_modules = ['STA-663-Final-Project-SSVD'],
    scripts = ['Start_Project.py'],
    data_files=[('my_data', ['data/breast_cancer.txt', 
                       'data/lung_cancer.txt',
                       'data/ssvd_results.txt'])],
    python_requires='>=3.6',
)
